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
        private object _itemsEnumetator;
        private object _f;

        public ExhaustiveRandomChoiceMonad(ICollectionModifier collectionModifier, Func<Tuple<bool, T>> toGetNextSource)
        {
            _toGetNextSource = toGetNextSource;
            _collectionModifier = collectionModifier;
        }

        public IChoiceMonad<TItem> Get<TItem>(Func<T, IEnumerable<TItem>> f) where TItem : class
        {
            if (_toGetNextSource == null) return new ExhaustiveRandomChoiceMonad<TItem>(_collectionModifier, null);
            if (_currentSource == null)
            {
                var tuple = _toGetNextSource();
                var isSucceeded = tuple.Item1;
                _currentSource = tuple.Item2;

                if (_currentSource == null) return new ExhaustiveRandomChoiceMonad<TItem>(_collectionModifier, null);
            }
            
            dynamic named = _currentSource;
            Console.WriteLine("...{0}:{1}", typeof(T).Name, named.Name);

            _f = f;
            var set = f(_currentSource);
            
            if (set != null && set.Any())
            {
                _itemsEnumetator = _collectionModifier.Modify(set).GetEnumerator();

                return new ExhaustiveRandomChoiceMonad<TItem>(_collectionModifier, GetNext<TItem>);
            }
            else
            {
                return TryNextSource<TItem>();
            }
        }


        private Tuple<bool, TItem> GetNext<TItem>() where TItem : class
        {
            var enumerator = (_itemsEnumetator as IEnumerator<TItem>);

            if (enumerator == null)
                return Tuple.Create<bool, TItem>(false, null);

            var ok = enumerator.MoveNext();

            if (!ok)
            {
                TryNextSource<TItem>();
                return GetNext<TItem>();
            }   

            return Tuple.Create(ok, enumerator.Current);
        }


        private IChoiceMonad<TItem> TryNextSource2<TItem>() where TItem : class
        {
            var tuple = _toGetNextSource();

            var isSucceeded = tuple.Item1;
            if (!isSucceeded)
                return new ExhaustiveRandomChoiceMonad<TItem>(_collectionModifier, null);

            _currentSource = tuple.Item2;

            return Get(_f as Func<T, IEnumerable<TItem>>);
        }

        private IChoiceMonad<TItem> TryNextSource<TItem>() where TItem : class
        {
            var tuple = _toGetNextSource();

            var isSucceeded = tuple.Item1;
            if (!isSucceeded)
                return new ExhaustiveRandomChoiceMonad<TItem>(_collectionModifier, null);

            _currentSource = tuple.Item2;

            return Get(_f as Func<T, IEnumerable<TItem>>);
        }

        public T Resolve()
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
    }
}