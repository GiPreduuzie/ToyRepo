using System.Collections.Generic;
using TreeBuilder.Nodes;

namespace TreeBuilder.Builders
{
    public class RootBuilder
    {
        private readonly List<Item> _items = new List<Item>();
        private readonly List<Folder> _folders = new List<Folder>();
        
        public FolderBuilder<RootBuilder> AddFolder(string name)
        {
            return new FolderBuilder<RootBuilder>(HostFolder, name);
        }

        public ItemBuilder<RootBuilder> AddItem(string name)
        {
            return new ItemBuilder<RootBuilder>(HostItem, name);
        }

        private RootBuilder HostItem(Item item)
        {
            _items.Add(item);
            return this;
        }

        private RootBuilder HostFolder(Folder folder)
        {
            _folders.Add(folder);
            return this;
        }

        public Root Done()
        {
            return new Root(_folders, _items);
        }
    }
}