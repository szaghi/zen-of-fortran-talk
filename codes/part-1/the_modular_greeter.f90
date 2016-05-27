module the_greeter_worker
contains
  subroutine say_something(what)
  character(*), intent(in) :: what !< What I have to say?
  print '(A)', what
  end subroutine say_something
endmodule the_greeter_worker

program the_modular_greeter
use the_greeter_worker
call say_something(what='Hello modular World!')
end program the_modular_greeter
