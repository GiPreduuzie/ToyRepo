using RandomChoiceMonad.CollectionModifiers;
using System;
using System.Collections.Generic;
using System.Linq;
using RandomChoiceMonad.Model;

namespace RandomChoiceMonad.RandomChoiceMonad
{
    public static class ExhaustiveRandomChoiceMonad
    {
        public static IChoiceMonad<T> Return<T>(T value, ICollectionModifier collectionModifier) where T : class
        {
            bool tried = false;
            return new ExhaustiveRandomChoiceMonad<T>(collectionModifier, () => {
                if (tried)
                {
                    return Tuple.Create<bool, T>(false, null);
                }
                else
                {
                    tried = true;
                    return Tuple.Create<bool, T>(true, value);
                }
            });
        }
    }

    class ExhaustiveRandomChoiceMonad<T> : IChoiceMonad<T> where T : class
    {
        private readonly Func<Tuple<bool, T>> _toGetNextSource;
        private readonly ICollectionModifier _collectionModifier;
        private T _currentSource; 
        private object _itemsEnumerator;

        public ExhaustiveRandomChoiceMonad(ICollectionModifier collectionModifier, Func<Tuple<bool, T>> toGetNextSource)
        {
            _toGetNextSource = toGetNextSource;
            _collectionModifier = collectionModifier;
        }

        public T Resolve()
        {
            return CurrentSource;
        }

        public IChoiceMonad<TItem> Get<TItem>(Func<T, IEnumerable<TItem>> f) where TItem : class
        {
            if (_toGetNextSource == null || CurrentSource == null)
                return new ExhaustiveRandomChoiceMonad<TItem>(_collectionModifier, null);

            _itemsEnumerator = Get(CurrentSource, f);

            if (_itemsEnumerator == null) return new ExhaustiveRandomChoiceMonad<TItem>(_collectionModifier, null);
            return new ExhaustiveRandomChoiceMonad<TItem>(_collectionModifier, () => GetNext(f));
        }

        private IEnumerator<TItem> Get<TItem>(T currentSource, Func<T, IEnumerable<TItem>> f) where TItem : class
        {
            dynamic named = currentSource;
            Console.WriteLine("...{0}:{1}", typeof(T).Name, named.Name);

            var set = f(currentSource);

            if (set != null && set.Any())
            {
                return _collectionModifier.Modify(set).GetEnumerator();
            }
            else
            {
                return TryNextSource(f);
            }
        }

        private T CurrentSource
        {
            get
            {
                if (_currentSource == null)
                {
                    var tuple = _toGetNextSource();
                    var isSucceeded = tuple.Item1;
                    _currentSource = tuple.Item2;

                    if (_currentSource == null)
                        return null;
                }
                return _currentSource;
            }
            set
            {
                _currentSource = value;
            }
        }


        private Tuple<bool, TItem> GetNext<TItem>(Func<T, IEnumerable<TItem>> f) where TItem : class
        {
            var enumerator = (_itemsEnumerator as IEnumerator<TItem>);

            if (enumerator == null)
                return Tuple.Create<bool, TItem>(false, null);

            var ok = enumerator.MoveNext();

            if (!ok)
            {
                var newEnumerator = TryNextSource(f);
                _itemsEnumerator = newEnumerator;
                return GetNext(f);
            }   

            return Tuple.Create(ok, enumerator.Current);
        }

        private IEnumerator<TItem> TryNextSource<TItem>(Func<T, IEnumerable<TItem>> f) where TItem : class
        {
            var tuple = _toGetNextSource();

            var isSucceeded = tuple.Item1;
            if (!isSucceeded)
                return null;

            CurrentSource = tuple.Item2;

            return Get(CurrentSource, f);
        }
    }
}