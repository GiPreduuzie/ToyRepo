using System;
using TreeBuilder.Nodes;

namespace TreeBuilder.Builders
{
    public class ItemBuilder<T>
    {
        private readonly string _name;
        private readonly Func<Item, T> _toHost;
        private string _description = "";
        

        public ItemBuilder(Func<Item, T> toHost, string name)
        {
            _name = name;
            _toHost = toHost;
        }

        public ItemBuilder<T> WithDescription(string description)
        {
            _description = description;
            return this;
        }

        public T Done()
        {
            return _toHost(new Item(_name, _description));
        }
    }
}