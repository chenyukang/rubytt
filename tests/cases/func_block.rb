def contrived(a, &f)
  # the block can be accessed through f
  f.call(a)

  # but yield also works !
  yield(a)
end

# this works
contrived(25) {|x| puts x}
