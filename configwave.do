onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /tb_Elevator_Controller_Systems/clock
add wave -noupdate /tb_Elevator_Controller_Systems/reset
add wave -noupdate /tb_Elevator_Controller_Systems/p
add wave -noupdate /tb_Elevator_Controller_Systems/q
add wave -noupdate /tb_Elevator_Controller_Systems/r
add wave -noupdate /tb_Elevator_Controller_Systems/s
add wave -noupdate /tb_Elevator_Controller_Systems/Led1
add wave -noupdate /tb_Elevator_Controller_Systems/Led2
add wave -noupdate /tb_Elevator_Controller_Systems/Led3
add wave -noupdate /tb_Elevator_Controller_Systems/Led4
add wave -noupdate /tb_Elevator_Controller_Systems/direction
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {20 ps} 0}
quietly wave cursor active 1
configure wave -namecolwidth 89
configure wave -valuecolwidth 109
configure wave -justifyvalue left
configure wave -signalnamewidth 1
configure wave -snapdistance 10
configure wave -datasetprefix 0
configure wave -rowmargin 4
configure wave -childrowmargin 2
configure wave -gridoffset 0
configure wave -gridperiod 300
configure wave -griddelta 40
configure wave -timeline 0
configure wave -timelineunits ps
update
WaveRestoreZoom {0 ps} {87 ps}
