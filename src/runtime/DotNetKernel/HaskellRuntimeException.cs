using System;

namespace Haskell.Runtime
{
	public class HaskellRuntimeException : Exception
	{
		public HaskellRuntimeException(string message) : base(message)
		{
		}
	}
}
