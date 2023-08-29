source env.sh


make BOARD=arty_a7_100 CONFIG=E300ArtyDevKitConfig -f Makefile.e300artydevkit clean
make BOARD=arty_a7_100 CONFIG=E300ArtyDevKitConfig -f Makefile.e300artydevkit verilog
make BOARD=arty_a7_100 CONFIG=E300ArtyDevKitConfig -f Makefile.e300artydevkit mcs

vivado -source upload_bitstream.tcl

rm -f vivado*
