using System;
using Microsoft.AspNetCore.JsonPatch;
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

            var patchWithExtra = new JsonPatchDocument()
                .Replace("/nested/prop", "bar")
                .Replace("/does/not/exist", "buz");

            var settings = new DiffSettings {
                IgnoreErrors = true,
            };
            
            Console.WriteLine(settings.SerializerSettings.DateParseHandling);
            Console.WriteLine(settings.SerializerSettings.DateTimeZoneHandling);

            // NOTE: this would error with IgnoreErrors set to true (the default)
            var patchedWithExtra = Diff.PatchWithJsonPatch(patchWithExtra, original, settings);
            Console.WriteLine(JsonConvert.SerializeObject(patchWithExtra));
        }
    }
}
