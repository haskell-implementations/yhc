using System;
using System.Reflection;

namespace Haskell.Runtime
{
	public class PAPClosure : Closure
	{
		ConstructorInfo constructor;
		Closure[] args;

		public PAPClosure(RuntimeMethodHandle hConstructor)
		{
			this.constructor = (ConstructorInfo) ConstructorInfo.GetMethodFromHandle(hConstructor);
			this.args        = new Closure[] {};
		}

		public PAPClosure(RuntimeMethodHandle hConstructor, Closure c1)
		{
			this.constructor = (ConstructorInfo) ConstructorInfo.GetMethodFromHandle(hConstructor);
			this.args        = new Closure[] {c1};
		}

		public PAPClosure(RuntimeMethodHandle hConstructor, Closure c1, Closure c2)
		{
			this.constructor = (ConstructorInfo) ConstructorInfo.GetMethodFromHandle(hConstructor);
			this.args        = new Closure[] {c1, c2};
		}

		public PAPClosure(RuntimeMethodHandle hConstructor, Closure c1, Closure c2, Closure c3)
		{
			this.constructor = (ConstructorInfo) ConstructorInfo.GetMethodFromHandle(hConstructor);
			this.args        = new Closure[] {c1, c2, c3};
		}

		public PAPClosure(RuntimeMethodHandle hConstructor, params Closure[] args)
		{
			this.constructor = (ConstructorInfo) ConstructorInfo.GetMethodFromHandle(hConstructor);
			this.args        = args;
		}

		public PAPClosure(ConstructorInfo constructor, params Closure[] args)
		{
			this.constructor = constructor;
			this.args        = args;
		}

		public override Closure Apply(params Closure[] extra)
		{
			int arity = constructor.GetParameters().Length;
			int given = args.Length + extra.Length;
			if (arity > given)
			{
				Closure[] new_args = new Closure[given];
				args.CopyTo(new_args,0);
				for (int i = args.Length; i < given; i++)
					new_args[i] = extra[i-args.Length];
				return new PAPClosure(constructor, new_args);
			}
			else
			{
				Closure[] new_args = new Closure[arity];
				args.CopyTo(new_args,0);
				for (int i = args.Length; i < arity; i++)
					new_args[i] = extra[i-args.Length];
				Closure c = (Closure) constructor.Invoke(new_args);

				if (arity < given)
				{
					Closure[] other_args = new Closure[given-arity];
					for (int i = 0; i < given-arity; i++)
						other_args[i] = extra[i+arity-args.Length];
					c = c.Apply(other_args);
				}

				return c;
			}
		}
	}
}
