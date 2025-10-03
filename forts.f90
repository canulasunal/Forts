module forts

    implicit none

    contains
        function replace(string, substring, new)
            character(len=*) :: string, substring, new
            character(len=256) :: part_one
            character(len=256) :: replace

            replace = trim(string)

            do
                if (index(replace, substring) == 0) then
                    exit
                else
                    part_one = trim(replace(index(replace, substring)+len(substring):len(trim(replace))))
                    replace = trim(replace(1:index(replace, substring)-1)) // new // trim(part_one)
                end if
            end do

        end function replace

        function startsWith(string, substring)
            character(len=*) :: string, substring
            integer :: startsWith

            if (index(string, substring) == 1) then
                startsWith = 1
            else
                startsWith = 0
            end if
        end function startsWith

end module forts