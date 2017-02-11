using System;
using System.Collections.Generic;
using TreeBuilder.Nodes;

namespace TreeBuilder.Builders
{
    public class FolderBuilder<T>
    {
        private readonly List<Item> _items = new List<Item>();
        private readonly List<Folder> _folders = new List<Folder>();
        private readonly Func<Folder, T> _toHost;
        private readonly string _name;

        public FolderBuilder(Func<Folder, T> toHost, string name)
        {
            _name = name;
            _toHost = toHost;
        }

        public FolderBuilder<FolderBuilder<T>> AddFolder(string name)
        {
            return new FolderBuilder<FolderBuilder<T>>(HostFolder, name);
        }

        public ItemBuilder<FolderBuilder<T>> AddItem(string name)
        {
            return new ItemBuilder<FolderBuilder<T>>(HostItem, name);
        }

        private FolderBuilder<T> HostItem(Item item)
        {
            _items.Add(item);
            return this;
        }

        private FolderBuilder<T> HostFolder(Folder folder)
        {
            _folders.Add(folder);
            return this;
        }
 
        public T Done()
        {
            return _toHost(new Folder(_name, _folders, _items));
        }
    }
}