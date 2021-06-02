%%raw(`
import { readAll } from "https://deno.land/std@0.97.0/io/util.ts"
`)

let read: string => Js.Promise.t<string> = %raw(`async function(path) {
  const file = await Deno.open(path, {read: true});
  const bytes = await readAll(file);
  Deno.close(file.rid);
  return new TextDecoder().decode(bytes)
}`)
