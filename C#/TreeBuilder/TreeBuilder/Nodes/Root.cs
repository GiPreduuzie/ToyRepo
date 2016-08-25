using System.Collections.Generic;
using System.Linq;

namespace TreeBuilder.Nodes
{
    public class Root : INode
    {
        public Root(IEnumerable<Folder> folders, IEnumerable<Item> items)
        {
            Items = items;
            Folders = folders;
        }

        public IEnumerable<Item> Items { get; private set; }
        public IEnumerable<Folder> Folders { get; private set; }

        public IEnumerable<INode> Children
        {
            get { return Folders.Cast<INode>().Union(Items); }
        }

        public override string ToString()
        {
            return FormatHelper.Join(
                "[root:",
                FormatHelper.JoinAndIdent(Folders),
                FormatHelper.JoinAndIdent(Items),
                "]");
        }
    }
}