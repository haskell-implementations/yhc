using System;
using System.IO;

namespace Haskell.Runtime
{
	/// <summary>
	/// Summary description for HandleClosure.
	/// </summary>
	public class HandleClosure : Closure
	{
		TextWriter writer;

		public HandleClosure(TextWriter writer)
		{
			this.writer = writer;
		}

		public void putChar(char c)
		{
			writer.Write(c);
		}
	}
}
