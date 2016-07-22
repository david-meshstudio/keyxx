contract MemberRegister
{
	struct Member
	{
		string name;
		uint balance;
	}
	mapping(bytes32 => Member) public memberList;
	mapping(bytes32 => address[]) public commitHistory;
	function Register(bytes32 _oid, string _name) public
	{
		memberList[_oid] = Member({
			name: _name,
			balance: 0
		});
	}
	function Deposite(bytes32 _oid, uint _amount) public
	{
		memberList[_oid].balance += _amount;
	}
	function Withdraw(bytes32 _oid, uint _amount) public
	{
		memberList[_oid].balance -= _amount;
	}
	function Record(bytes32 _oid, address _commit) public
	{
		commitHistory[_oid].push(_commit);
	}
}