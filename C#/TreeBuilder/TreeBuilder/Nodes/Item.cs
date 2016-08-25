using System.Collections.Generic;

namespace TreeBuilder.Nodes
{
    public class Item : INode
    {
        public Item(string name, string description)
        {
            Name = name;
            Description = description;
        }

        public string Name { get; private set; }
        public string Description { get; private set; }

        public IEnumerable<INode> Children
        {
            get { return new List<INode>(); }
        }

        public override string ToString()
        {
            return string.Format("[item: {0}, {1}]", Name, string.IsNullOrWhiteSpace(Description)? "<no description>" : Description);
        }
    }
}