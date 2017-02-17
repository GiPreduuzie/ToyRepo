using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Wrapped;

namespace TemplatesForProperties
{
    public class MyClass
    {
        public string Property1 { get; set; }
        public string Property2 { get; set; }
    }


    class Program
    {
        static void Do(MyProperty<string> property)
        {
            Console.WriteLine(property.Get());
            property.Set("new value");
            Console.WriteLine(property.Get());
        }

        static void Main(string[] args)
        {
            Console.WriteLine(typeof(MyClass).FullName);
            foreach(var property in typeof(MyClass).GetProperties())
            {
                Console.WriteLine(property.PropertyType);
                //Console.WriteLine(property.ReflectedType);

            }

            Do(new MyClassWrapper(new MyClass() { Property1 = "Hello!" }).Property1);
        }
    }
}
