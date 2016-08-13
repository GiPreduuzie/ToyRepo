using RandomChoiceMonad.CollectionModifiers;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RandomChoiceMonad.RandomChoiceMonad
{
    class StateMonad<TValue, TState>
    {
        public StateMonad(TValue value, TState state)
        {
            Value = value;
            State = state;
        }

        public TValue Value { get; private set; }
        public TState State { get; private set; }

        public StateMonad<TNew, TState> Map<TNew>(Func<TValue, TNew> f)
        {
            return new StateMonad<TNew, TState>(f(Value), State);
        }

        public StateMonad<TNew, TState> Bind<TNew>(Func<TValue, StateMonad<TNew, TState>> f)
        {
            return f(Value);
        }
    }

    public static class ExhaustiveRandomChoiceMonad
    {
        public static IChoiceMonad<T> Return<T>(
            T value,
            ICollectionModifier collectionModifier,
            StringBuilder logger) where T : class
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
            },
            logger);
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
        private readonly StringBuilder _logger;

        public ExhaustiveRandomChoiceMonad(
            ICollectionModifier collectionModifier,
            Func<Tuple<bool, T>> toGetNextSource,
            StringBuilder logger)
        {
            if (collectionModifier == null)
                throw new ArgumentNullException(nameof(collectionModifier));

            if (toGetNextSource == null)
                throw new ArgumentNullException(nameof(toGetNextSource));

            _toGetNextSource = toGetNextSource;
            _collectionModifier = collectionModifier;
            _logger = logger;
        }

        public T Resolve()
        {
            return CurrentSource;
        }

        public IChoiceMonad<TItem> Get<TItem>(Func<T, IEnumerable<TItem>> f) where TItem : class
        {
            if (_toGetNextSource == null || CurrentSource == null)
                return new ExhaustiveRandomChoiceMonad<TItem>(_collectionModifier, null, _logger);

            var resultPack = GetEnumerator(CurrentSource, _collectionModifier, f);
            CurrentSource = resultPack.CurrentSource;
            var itemsEnumerator = resultPack.Enumerator;
            
            return new ExhaustiveRandomChoiceMonad<TItem>(_collectionModifier, 
                () =>
            {
                var result = GetNext(itemsEnumerator, f, CurrentSource);
                CurrentSource = result.Item3;

                return Tuple.Create(result.Item1, result.Item2);
            },
            _logger);
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

        private ResultPack<T, TItem> GetEnumerator<TItem>(
            T currentSource, 
            ICollectionModifier collectionModifier,
            Func<T, IEnumerable<TItem>> f) 
            where TItem : class
        {
            dynamic named = currentSource;
            _logger.AppendLine(string.Format("...{0}:{1}", typeof(T).Name, named.Name));

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

        private Tuple<bool, TItem, T> GetNext<TItem>(
            IEnumerator<TItem> enumerator,
            Func<T, IEnumerable<TItem>> f,
            T currentSource)
            where TItem : class
        {
            if (enumerator == null)
                return Tuple.Create<bool, TItem, T>(false, null, currentSource);

            var currentResultPack = new ResultPack<T, TItem>(enumerator, currentSource);
            Tuple<bool, TItem> result = null;
            while (result == null)
            {
                if (currentResultPack.Enumerator == null)
                {
                    result = Tuple.Create<bool, TItem>(false, null);
                }
                else
                {
                    if (currentResultPack.Enumerator.MoveNext())
                    {
                        result = Tuple.Create(true, currentResultPack.Enumerator.Current);
                    }
                    else
                    {
                        currentResultPack = TryNextSource(f);
                    }
                }
            }

            return Tuple.Create(result.Item1, result.Item2, currentResultPack.CurrentSource);
        }

        private ResultPack<T, TItem> TryNextSource<TItem>(Func<T, IEnumerable<TItem>> f) where TItem : class
        {
            var tuple = _toGetNextSource();
            return  tuple.Item1 
                ? GetEnumerator(tuple.Item2, _collectionModifier, f) 
                : new ResultPack<T, TItem>(null, tuple.Item2);
        }
    }
}