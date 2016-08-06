using System;
using System.Collections.Generic;
using System.Linq;

namespace RandomChoiceMonad
{
    static class RandomChoiceMonad
    {
        public static RandomChoiceMonad<T> Return<T>(T value, Random random) where T : class
        {
            return new RandomChoiceMonad<T>(random, value);
        }
    }

    class RandomChoiceMonad<T> where T : class
    {
        private readonly T _value;
        private readonly Random _random;

        public RandomChoiceMonad(Random random, T value)
        {
            _value = value;
            _random = random;
        }

        public RandomChoiceMonad<TItem> Get<TItem>(Func<T, IEnumerable<TItem>> f) where TItem : class
        {
            if (_value == null)
                return new RandomChoiceMonad<TItem>(_random, null);

            var set = f(_value)?.ToArray();

            return set != null && set.Any()
                ? new RandomChoiceMonad<TItem>(_random, set[_random.Next(set.Length)])
                : new RandomChoiceMonad<TItem>(_random, null);
        }

        internal T Resolve()
        {
            return _value;
        }
    }
}