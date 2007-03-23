x = 10;

switch (x) 
{

	case 5:
		intrinsic::assert(false);

	case (3 * 3 + 1): 
		intrinsic::assert(true);
}

switch ("hello" + "there")
{
	case 10:
		intrinsic::assert(false);

	case "hellothere":
		intrinsic::assert(true);
}

switch (5)
{
	case 10:
		intrinsic::assert(false);

	default:
		intrinsic::assert(true);
}
		
