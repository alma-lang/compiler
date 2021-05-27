@scope("Deno") @val
external test: (string, unit => unit) => unit = "test"

@scope("Deno") @val
external testAsync: (string, unit => Js.Promise.t<unit>) => unit = "test"

@module("https://deno.land/std@0.97.0/testing/asserts.ts")
external equal: ('a, 'a) => bool = "equal"

@module("https://deno.land/std@0.97.0/testing/asserts.ts")
external assertTrue: (bool, string) => unit = "assert"

@module("https://deno.land/std@0.97.0/testing/asserts.ts")
external assertEquals: ('a, 'a, string) => unit = "assertEquals"

@module("https://deno.land/std@0.97.0/testing/asserts.ts")
external assertNotEquals: ('a, 'a, string) => unit = "assertNotEquals"

@module("https://deno.land/std@0.97.0/testing/asserts.ts")
external fail: string => unit = "fail"

type testFunctions = {
  test: (string, unit => unit) => unit,
  testAsync: (string, unit => Js.Promise.t<unit>) => unit,
}

let suite = (scope: string, testFn: testFunctions => unit): unit => {
  let testFns: testFunctions = {
    test: (subject, fn) => test(`${scope}: ${subject}`, fn),
    testAsync: (subject, fn) => testAsync(`${scope}: ${subject}`, fn),
  }
  testFn(testFns)
}

/* export function assertStrictEquals( */
/* actual: unknown, */
/* expected: unknown, */
/* msg?: string, */
/* ): void; */
/* export function assertStrictEquals<T>( */
/* actual: T, */
/* expected: T, */
/* msg?: string, */
/* ): void; */
/* export function assertStrictEquals( */
/* actual: unknown, */
/* expected: unknown, */
/* msg?: string, */
/* ): void */

/* export function assertNotStrictEquals( */
/* actual: unknown, */
/* expected: unknown, */
/* msg?: string, */
/* ): void; */
/* export function assertNotStrictEquals<T>( */
/* actual: T, */
/* expected: T, */
/* msg?: string, */
/* ): void; */
/* export function assertNotStrictEquals( */
/* actual: unknown, */
/* expected: unknown, */
/* msg?: string, */
/* ): void */

/* export function assertExists( */
/* actual: unknown, */
/* msg?: string, */
/* ): void */

/* export function assertStringIncludes( */
/* actual: string, */
/* expected: string, */
/* msg?: string, */
/* ): void */

/* export function assertArrayIncludes( */
/* actual: ArrayLike<unknown>, */
/* expected: ArrayLike<unknown>, */
/* msg?: string, */
/* ): void; */
/* export function assertArrayIncludes<T>( */
/* actual: ArrayLike<T>, */
/* expected: ArrayLike<T>, */
/* msg?: string, */
/* ): void; */
/* export function assertArrayIncludes( */
/* actual: ArrayLike<unknown>, */
/* expected: ArrayLike<unknown>, */
/* msg?: string, */
/* ): void */

/* export function assertMatch( */
/* actual: string, */
/* expected: RegExp, */
/* msg?: string, */
/* ): void */

/* export function assertNotMatch( */
/* actual: string, */
/* expected: RegExp, */
/* msg?: string, */
/* ): void */

/* export function assertObjectMatch( */
/* // deno-lint-ignore no-explicit-any */
/* actual: Record<PropertyKey, any>, */
/* expected: Record<PropertyKey, unknown>, */
/* ): void */

/* export function assertThrows<T = void>( */
/* fn: () => T, */
/* ErrorClass?: Constructor, */
/* msgIncludes = "", */
/* msg?: string, */
/* ): Error */

/* export async function assertThrowsAsync<T = void>( */
/* fn: () => Promise<T>, */
/* ErrorClass?: Constructor, */
/* msgIncludes = "", */
/* msg?: string, */
/* ): Promise<Error> */

/* export function unimplemented(msg?: string): never */

/* export function unreachable(): never */
