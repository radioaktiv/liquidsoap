# End test successfully.
def test.pass()
  shutdown()
end

# End test with a failure.
def test.fail()
  exit(1)
end

# End test with signal 2
def test.skip()
  exit(2)
end

# Check that files are never repeated in source s, possibly by rounds. The
# function triggers test.fail on repeated filenames, only clearing its list of
# seen filenames once all nb_files have been seen.
def test.check_non_repeating(~nb_files,~nb_rounds,s)
  # List of seen filenames
  seen = ref([])
  # Number of rounds to test
  iterations = ref(0)

  def already(fname)
    list.assoc(default=false, fname, !seen)
  end

  def check(m)
    fname = m["filename"]
    print("I: Playing #{fname}")
    if !iterations < nb_rounds and already(fname) then
      print("I: Already seen #{fname}")
      test.fail()
    else
      if list.length(!seen) < nb_files-1 then
        seen := list.add((fname,true),!seen)
      else
        print("I: ===")
        seen := []
        iterations := !iterations+1
        if !iterations == nb_rounds then
          print("I: Test passed")
          test.pass()
        end
      end
    end
  end

  on_track(id="check_non_repeating",check,s)
end
