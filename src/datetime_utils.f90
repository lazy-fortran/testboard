module datetime_utils
  !! Date and time utilities
  implicit none
  private

  public :: get_iso8601_timestamp

contains

  function get_iso8601_timestamp() result(timestamp)
    !! Get current timestamp in ISO 8601 format (UTC)
    character(len=25) :: timestamp
    integer :: values(8)
    character(len=4) :: year
    character(len=2) :: month, day, hour, minute, second

    call date_and_time(values=values)

    write(year, '(I4.4)') values(1)
    write(month, '(I2.2)') values(2)
    write(day, '(I2.2)') values(3)
    write(hour, '(I2.2)') values(5)
    write(minute, '(I2.2)') values(6)
    write(second, '(I2.2)') values(7)

    timestamp = year // '-' // month // '-' // day // 'T' // &
                hour // ':' // minute // ':' // second // 'Z'
  end function get_iso8601_timestamp

end module datetime_utils
