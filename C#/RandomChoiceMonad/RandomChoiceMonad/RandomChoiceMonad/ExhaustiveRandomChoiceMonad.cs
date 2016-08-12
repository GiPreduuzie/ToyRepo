using System;
using System.Collections.Generic;
using System.Linq;

namespace RandomChoiceMonad
{
    static class ExhaustiveRandomChoiceMonad
    {
        public static ExhaustiveRandomChoiceMonad<T> Return<T>(T value, Random random) where T : class
        {
            bool tried = false;
            return new ExhaustiveRandomChoiceMonad<T>(random, () => {
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

    class ExhaustiveRandomChoiceMonad<T> where T : class
    {
        private readonly Func<Tuple<bool, T>> _toGetNextSource;
        private readonly Random _random;
        private T _currentSource; 
        private object _itemsEnumetator;
        private object _f;

        public ExhaustiveRandomChoiceMonad(Random random, Func<Tuple<bool, T>> toGetNextSource)
        {
            
            _toGetNextSource = toGetNextSource;
            _random = random;
        }

        public ExhaustiveRandomChoiceMonad<TItem> Get<TItem>(Func<T, IEnumerable<TItem>> f) where TItem : class
        {
            if (_toGetNextSource == null) return new ExhaustiveRandomChoiceMonad<TItem>(_random, null);
            if (_currentSource == null)
            {
                var tuple = _toGetNextSource();
                var isSucceeded = tuple.Item1;
                _currentSource = tuple.Item2;

                if (_currentSource == null) return new ExhaustiveRandomChoiceMonad<TItem>(_random, null);
            }
            
            dynamic named = _currentSource;
            Console.WriteLine("...{0}:{1}", typeof(T).Name, named.Name);

            _f = f;
            var set = f(_currentSource);
            
            if (set != null && set.Any())
            {
                // stay in current position
                var randomizedSet = set;
                _itemsEnumetator = randomizedSet.GetEnumerator();

                return new ExhaustiveRandomChoiceMonad<TItem>(_random, GetNext<TItem>);
            }
            else
            {
                return TryNextSource<TItem>();
            }
        }

        private IEnumerable<TItem> Randomize<TItem>(IEnumerable<TItem> set) where TItem : class
        {
            var list = set.ToList();

            while (list.Any())
            {
                var next = _random.Next(list.Count);
                var result = list[next];
                list.RemoveAt(next);
                yield return result;
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


        private ExhaustiveRandomChoiceMonad<TItem> TryNextSource2<TItem>() where TItem : class
        {
            var tuple = _toGetNextSource();

            var isSucceeded = tuple.Item1;
            if (!isSucceeded)
                return new ExhaustiveRandomChoiceMonad<TItem>(_random, null);

            _currentSource = tuple.Item2;

            return Get(_f as Func<T, IEnumerable<TItem>>);
        }

        private ExhaustiveRandomChoiceMonad<TItem> TryNextSource<TItem>() where TItem : class
        {
            var tuple = _toGetNextSource();

            var isSucceeded = tuple.Item1;
            if (!isSucceeded)
                return new ExhaustiveRandomChoiceMonad<TItem>(_random, null);

            _currentSource = tuple.Item2;

            return Get(_f as Func<T, IEnumerable<TItem>>);
        }

        internal T Resolve()
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