using System;

namespace Haskell.Runtime
{
	public class CAFClosure : Closure
	{
		public CAFClosure()
		{
		}

		public override Closure Apply(Closure c1)
		{
			return Eval().Apply(c1);
		}

		public override Closure Apply(Closure c1, Closure c2)
		{
			return Eval().Apply(c1, c2);
		}

		public override Closure Apply(Closure c1, Closure c2, Closure c3)
		{
			return Eval().Apply(c1, c2, c3);
		}

		public override Closure Apply(Closure[] args)
		{
			return Eval().Apply(args);
		}
	}
}
