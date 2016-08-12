using System.Collections.Generic;

namespace RandomChoiceMonad.CollectionModifiers
{
    class IdenticalCollectionModifier : ICollectionModifier
    {
        public IEnumerable<T> Modify<T>(IEnumerable<T> collection)
        {
            return collection;
        }
    }
}
