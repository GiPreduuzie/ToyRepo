

using System;
using TemplatesForProperties;

namespace Wrapped{

public class MyProperty<T>
{
    T _embedded;

    public MyProperty(Func<T> get, Action<T> set)
	{
	    Set = set;
		Get = get;
	}

    public Action<T> Set { get; }
	public Func<T> Get { get; }
}

public class MyClassWrapper
{
	MyClass _embedded;

    public MyClassWrapper(MyClass embedded)
	{
	    _embedded = embedded;
	}

  
   public MyProperty<System.String> Property1 =>
       new MyProperty<System.String>(() => _embedded.Property1, v => _embedded.Property1 = v );
  
   public MyProperty<System.String> Property2 =>
       new MyProperty<System.String>(() => _embedded.Property2, v => _embedded.Property2 = v );
  

}
}