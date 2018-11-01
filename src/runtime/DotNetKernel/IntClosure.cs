using System;

namespace Haskell.Runtime
{
	public class IntClosure : Closure
	{
		private int val;

		public IntClosure(int val)
		{
			this.val = val;
		}

		public int Value
		{
			get { return val; }
		}
	}
}
