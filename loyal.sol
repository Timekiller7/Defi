pragma solidity ^0.8.13;

import "@openzeppelin/contracts/access/AccessControl.sol";

contract Loyal is AccessControl {
    uint256 private constant INITIAL_RATE = 10**11; // = 1 бонус
    uint256 private constant MAX_RATE = INITIAL_RATE + INITIAL_RATE/5;    //120% от INITIAL_RATE (20/100 = 1/5)
    uint256 private constant MIN_RATE = INITIAL_RATE - INITIAL_RATE/5;

    uint256 private _rate = INITIAL_RATE; 
    uint256 private lastRateUpdate;

    uint256 private lastActivityUpdate;
    uint256 private _participants; // для usedBonus
    
    uint256 private constant PERC_FOR_ACTIVITY = 30;
    uint256 private constant PERC_HUNDRED = 100;
    bytes32 public constant MANAGER_ROLE = keccak256("MANAGER_ROLE");
    bytes32 public constant USER_ROLE = keccak256("USER_ROLE");

    mapping(address => uint256) private _loyalty;

    mapping(uint256 => address) private _participant;
    mapping(address => uint256) private _usedBonus;
    
    event addedBonus(address indexed user, uint256 added);
    event usedBonus(address indexed user, uint256 used);
    event transferredActivityBonus(address indexed to, uint256 amount);
    event rateChanged(uint256 newRate, uint256 time);
    event weiConverted(address indexed user, uint256 _wei, uint256 rate);
    event withdrawned(address indexed to, uint256 amount);
    event addedManager(address indexed manager);
    event deletedManager(address indexed manager);
    event addedUser(address indexed user, uint256 withBonus);
    event deletedUser(address indexed user);

    constructor(address[] memory managers) {  
        _setupRole(DEFAULT_ADMIN_ROLE, msg.sender);
        uint256 len = managers.length;
        for(uint256 i=0;i<len; i++) {
            grantRole(MANAGER_ROLE,managers[i]);
        }
        _setRoleAdmin(USER_ROLE, MANAGER_ROLE);
        lastActivityUpdate = block.timestamp;
        lastRateUpdate = block.timestamp;
    }

    // геттеры: информация о контракте
    function getRateInfo() public view returns(uint256,uint256,uint256,uint256,uint256) {
        return(_rate,lastRateUpdate,INITIAL_RATE,MAX_RATE,MIN_RATE);
    }
       
    function getActivityInfo() public view returns(uint256,uint256,uint256) {
        return(PERC_FOR_ACTIVITY,_participants,lastActivityUpdate);
    }

    // только DEFAULT_ADMIN_ROLE вызывает
    function addManager(address manager) external { 
        require(!hasRole(USER_ROLE,manager));
        grantRole(MANAGER_ROLE, manager);
        emit addedManager(manager);
    }

    function deleteManagers(address manager) external {
        revokeRole(MANAGER_ROLE, manager);
        emit deletedManager(manager);
    }

    function changeRate (uint256 newRate) external onlyRole(DEFAULT_ADMIN_ROLE) { 
        require(newRate >= MIN_RATE && newRate <= MAX_RATE,
            "Inappropriate new rate"
        );
        require(lastRateUpdate + 20 days <= block.timestamp,
            "Too soon for rate updating"
        );

        _rate = newRate;
        emit rateChanged(newRate, block.timestamp);
    }

    function transferBonusForActiv() external onlyRole(DEFAULT_ADMIN_ROLE) {
        require(lastActivityUpdate + 31 days <= block.timestamp,
            "Too soon for activity bonus transferring"
        );                                               
        uint256 len = _participants;
        uint256 biggestSum; 
        address winner;

        for(uint256 i=0; i<len; i++) {
            address participant = _participant[i];
            if(biggestSum < _usedBonus[participant] && 
                hasRole(USER_ROLE,participant)
                ) 
            {
                biggestSum = _usedBonus[participant];
                winner = participant;
            } 
        }
        
        uint256 amount = (biggestSum * PERC_FOR_ACTIVITY)/PERC_HUNDRED;
        _loyalty[winner] += amount;
        lastActivityUpdate = block.timestamp;

        emit transferredActivityBonus(winner, amount);

        resetParticipants();
    }

    function withdraw(address to, uint256 amount) external onlyRole(DEFAULT_ADMIN_ROLE) {
        require(getBalance() >= amount,
            "Insufficient balance for withdrawing"
        );

        (bool success, ) = payable(to).call{value: amount}("");
        require(success, 
           "Failed to transfer Ether to admin"
        );

        emit withdrawned(to,amount);
    }

    // только MANAGER_ROLE вызывает
    function addUser(address user, uint256 bonus) external {  
        require(!hasRole(MANAGER_ROLE,user), 
            "Manager can't be user"
        );

        grantRole(USER_ROLE, user);
        if(bonus != 0) 
            _loyalty[user] += bonus;
        emit addedUser(user,bonus);
    }
    
    function deleteUser(address user) external { // даже если пользователь участвует в акции "30% самому активному пользователю", он не победит, тк нет роли USER_ROLE. Потом вся инфа про него сотрется
        _loyalty[user] = 0;              
        revokeRole(USER_ROLE, user);

        emit deletedUser(user);
    }

    function addBonus(address user, uint256 bonus) public onlyRole(MANAGER_ROLE) {
        require(hasRole(USER_ROLE, user),
            "User has to be in loyal system"
        );

        _loyalty[user] += bonus;
        emit addedBonus(user,bonus);
    }
    
    // только USER_ROLE может вызывать
    function convertWeiToBonus() external payable onlyRole(USER_ROLE) {  // минимально можно получить 1 бонус
        uint256 amount = msg.value;
        uint256 rate = _rate;
        require(amount >= rate,
            "Insufficient amount"
        );
        
        _loyalty[msg.sender] += amount / rate;
        
        emit weiConverted(msg.sender,amount,rate);
    } 

    function useBonus(uint256 amount) public onlyRole(USER_ROLE) {
        require(amount <= _loyalty[msg.sender], 
            "Insufficient amount of bonuses to use"
        );

        _loyalty[msg.sender] -= amount;
        _usedBonus[msg.sender] += amount;

        if(_participant[_participants] == address(0)) {
            _participant[_participants] = msg.sender;
            _participants += 1;
        }
        
        emit usedBonus(msg.sender, amount);
    }


    // геттеры
    function getBalance() public view returns(uint256) {
        return address(this).balance;
    }

    function getBonuses(address user) public view returns(uint256) {
        return _loyalty[user];
    }

    function getUsedBonus(address user) public view returns(uint256) {
        return _usedBonus[user];
    }

    function getParticipant(uint256 number) public view returns(address) {
        return _participant[number];
    }

    function getParticipants() public view returns(uint256) {
        return _participants;
    }

    function resetParticipants() private {
        uint256 len = _participants;
        _participants = 0;
        for(uint256 i=0; i<len; i++) {
            _usedBonus[_participant[i]] = 0;
            _participant[i] = address(0);
        }
    }         
}
