using System;

namespace Haskell.Runtime
{
	public class PatternMatchException : HaskellRuntimeException
	{
		public PatternMatchException() : base("Pattern match failure")
		{
		}
	}
}
