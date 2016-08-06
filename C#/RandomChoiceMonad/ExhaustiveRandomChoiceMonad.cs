using System;
using System.Collections.Generic;
using System.Linq;

namespace RandomChoiceMonad
{
    static class ExhaustiveRandomChoiceMonad
    {
        public static ExhaustiveRandomChoiceMonad<T> Return<T>(T value, Random random) where T : class
        {
            return new ExhaustiveRandomChoiceMonad<T>(random, new[] { value });
        }  
    }

    class ExhaustiveRandomChoiceMonad<T> where T : class
    {
        private readonly IEnumerable<T> _values;
        private readonly Random _random;

        public ExhaustiveRandomChoiceMonad(Random random, IEnumerable<T> values)
        {
            _values = values;
            _random = random;
        }

        public ExhaustiveRandomChoiceMonad<TItem> Get<TItem>(Func<T, IEnumerable<TItem>> f) where TItem : class
        {
            if (_values == null)
                return new ExhaustiveRandomChoiceMonad<TItem>(_random, null);

            var set = _values.Select(x => f(x)?.ToArray()).FirstOrDefault(x => x != null && x.Any());

            return set ==  null 
                ? new ExhaustiveRandomChoiceMonad<TItem>(_random, null) 
                : new ExhaustiveRandomChoiceMonad<TItem>(_random, set);
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
            
        internal T Resolve()
        {
            return _values?.FirstOrDefault();
        }
    }
}