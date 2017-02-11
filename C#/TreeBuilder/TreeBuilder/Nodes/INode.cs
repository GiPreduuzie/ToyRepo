using System.Collections.Generic;

namespace TreeBuilder.Nodes
{
    public interface INode
    {
        IEnumerable<INode> Children { get; }
    }
}