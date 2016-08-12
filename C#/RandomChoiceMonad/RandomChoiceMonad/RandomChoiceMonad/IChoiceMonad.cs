using System;
using System.Collections.Generic;

namespace RandomChoiceMonad.RandomChoiceMonad
{
    public interface IChoiceMonad<T> where T : class
    {
        IChoiceMonad<TItem> Get<TItem>(Func<T, IEnumerable<TItem>> f) where TItem : class;
        T Resolve();
    }
}