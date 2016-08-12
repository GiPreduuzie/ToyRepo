using System.Collections.Generic;

namespace RandomChoiceMonad.CollectionModifiers
{
    public interface ICollectionModifier
    {
        IEnumerable<T> Modify<T>(IEnumerable<T> collection);
    }
}
