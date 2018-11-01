using System;

namespace Haskell.Runtime
{
	public class BlackHoleClosure : Closure
	{
		public static Closure indirection = new BlackHoleClosure();

		public BlackHoleClosure()
		{
		}

		public override Closure Eval()
		{
			throw new HaskellRuntimeException("Black hole entered.");
		}
	}
}
