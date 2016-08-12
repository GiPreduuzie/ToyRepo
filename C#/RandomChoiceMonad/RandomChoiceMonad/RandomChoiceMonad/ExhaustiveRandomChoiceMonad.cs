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

    class ResultPack<T, TItem>
    {
        public ResultPack(
            IEnumerator<TItem> enumerator,
            T currentSource)
        {
            Enumerator = enumerator;
            CurrentSource = currentSource;
        }

        public IEnumerator<TItem> Enumerator { get; private set; }
        public T CurrentSource { get; private set; }
    }

    class ExhaustiveRandomChoiceMonad<T> : IChoiceMonad<T> where T : class
    {
        private readonly Func<Tuple<bool, T>> _toGetNextSource;
        private readonly ICollectionModifier _collectionModifier;
        private T _currentSource; 

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

            var resultPack = GetEnumerator(CurrentSource, _collectionModifier, f);
            CurrentSource = resultPack.CurrentSource;
            var itemsEnumerator = resultPack.Enumerator;

            if (itemsEnumerator == null) return new ExhaustiveRandomChoiceMonad<TItem>(_collectionModifier, null);
            return new ExhaustiveRandomChoiceMonad<TItem>(_collectionModifier, () => GetNext(itemsEnumerator, f));
        }

        private ResultPack<T, TItem> GetEnumerator<TItem>(
            T currentSource, 
            ICollectionModifier collectionModifier,
            Func<T, IEnumerable<TItem>> f) 
            where TItem : class
        {
            dynamic named = currentSource;
            Console.WriteLine("...{0}:{1}", typeof(T).Name, named.Name);

            var set = f(currentSource);

            if (set != null && set.Any())
            {
                return new ResultPack<T, TItem>(collectionModifier.Modify(set).GetEnumerator(), currentSource);
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


        private Tuple<bool, TItem> GetNext<TItem>(IEnumerator<TItem> enumerator, Func<T, IEnumerable<TItem>> f) where TItem : class
        {
            if (enumerator == null)
                return Tuple.Create<bool, TItem>(false, null);

            if (enumerator.MoveNext())
            {
                return Tuple.Create(true, enumerator.Current);
            }
            else
            {
                var result = TryNextSource(f);
                CurrentSource = result.CurrentSource;
                
                return GetNext(result.Enumerator, f);
            }
        }

        private ResultPack<T, TItem> TryNextSource<TItem>(Func<T, IEnumerable<TItem>> f) where TItem : class
        {
            var tuple = _toGetNextSource();
            return  tuple.Item1 ? GetEnumerator(tuple.Item2, _collectionModifier, f) : null;
        }
    }
}