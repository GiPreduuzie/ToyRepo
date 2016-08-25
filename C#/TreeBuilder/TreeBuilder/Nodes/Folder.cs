using System.Collections.Generic;
using System.Linq;

namespace TreeBuilder.Nodes
{
    public class Folder : INode
    {
        public Folder(string name, IEnumerable<Folder> folders, IEnumerable<Item> items)
        {
            Name = name;
            Items = items;
            Folders = folders;
        }

        public string Name {get; private set; }
        public IEnumerable<Item> Items { get; private set; }
        public IEnumerable<Folder> Folders { get; private set; }

        public IEnumerable<INode> Children
        {
            get { return Folders.Cast<INode>().Union(Items); }
        }

        public override string ToString()
        {
            return FormatHelper.Join(
                "[folder: " + Name,
                FormatHelper.JoinAndIdent(Folders),
                FormatHelper.JoinAndIdent(Items),
                "]");
        }
    }
}