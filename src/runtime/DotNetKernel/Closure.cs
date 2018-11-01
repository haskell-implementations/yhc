using System;

namespace Haskell.Runtime
{
	public class Closure
	{
		public Closure()
		{
		}

		public virtual Closure Eval()
		{
			return this;
		}

		public int EvalInt()
		{
			return ((IntClosure) Eval()).Value;
		}

		public virtual Closure Apply(Closure c1)
		{
			return Apply(new Closure[] {c1});
		}

		public virtual Closure Apply(Closure c1, Closure c2)
		{
			return Apply(new Closure[] {c1, c2});
		}

		public virtual Closure Apply(Closure c1, Closure c2, Closure c3)
		{
			return Apply(new Closure[] {c1, c2, c3});
		}

		public virtual Closure Apply(Closure[] args)
		{
			throw new HaskellRuntimeException("This isn't a PAP closure");
		}
	}
}
