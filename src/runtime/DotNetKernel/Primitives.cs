using System;

namespace Haskell.Runtime
{
	public class Primitives
	{
		public static Closure StdoutThunk()
		{
			return new HandleClosure(Console.Out);
		}

		public static Closure StderrThunk()
		{
			return new HandleClosure(Console.Error);
		}

		public static Closure HGetErrorCThunk(Closure c)
		{
			return new IntClosure(0);
		}

		public static Closure HPutCharCThunk(Closure c1, Closure c2)
		{
			((HandleClosure) c1).putChar((char) ((IntClosure) c2).Value);
			return null;
		}

		public static Closure PrimCreateForeignPtrThunk(Closure c)
		{
			return c;
		}
	}
}
