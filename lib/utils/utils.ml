let code =
  let text =
{|try:
  hello = 13
  def absolute(x):
    if (x < 0): return -x
    else: x
  a = absolute(absolute(10))
except:
  pass

def absolute2(x):
  if (x < 0): return -x
  else: x
print(a)
n = absolute2(absolute2(10))
  
|} in
  let clean s = String.to_seq s
  |> Seq.filter (fun c -> c <> '\r')
  |> String.of_seq in
  clean text


let error = {|
  Traceback (most recent call last):
    File "/mnt/c/Workspace/AITA/project/test_ocaml/hello.py", line 16, in <module>
      print(a)
  NameError: name 'a' is not defined
  |}
