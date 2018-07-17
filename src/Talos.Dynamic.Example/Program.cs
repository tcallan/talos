using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using System.Runtime.Serialization;
using Newtonsoft.Json;

namespace Talos.Dynamic.Example
{
    internal class NestedContract
    {
        [JsonProperty("prop")]
        public readonly string StringProp;

        public NestedContract(string stringProp)
        {
            StringProp = stringProp;
        }
    }

    internal class Contract
    {
        [JsonProperty("prop")]
        public readonly string Prop;

        [JsonProperty("nested")]
        public readonly NestedContract NestedContract;

        public Contract(string prop, NestedContract nestedContract)
        {
            Prop = prop;
            NestedContract = nestedContract;
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            var original = new Contract("foo", new NestedContract("bar"));
            var updated = new Contract("baz", new NestedContract("buz"));

            var patch = Diff.DiffToJsonPatch(original, updated);
            Console.WriteLine(JsonConvert.SerializeObject(patch));

            var patched = Diff.PatchWithJsonPatch(patch, original);
            Console.WriteLine(JsonConvert.SerializeObject(original));
            Console.WriteLine(JsonConvert.SerializeObject(patched));

            var differ = new Diff.DifferBuilder<Contract>()
                .WithFilter(o => o)
                .Build();

            var patch2 = differ.DiffToJsonPatch(original, updated);
            Console.WriteLine(JsonConvert.SerializeObject(patch2));
        }
    }
}
