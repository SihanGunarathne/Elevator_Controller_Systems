`timescale 1ms/1ps

module Elevator_Controller_Systems (Flr1, Flr2, Flr3, Flr4,A,B,Flr1_up, Flr2_up, Flr2_down, Flr3_up, Flr3_down, Flr4_down, Led1, Led2, Led3, Led4, sevseg, clock, Door, reset,
 direction, rest);
  
 
 output wire [0:6] sevseg;
 //output wire [0:6] segout;
 
 output wire  Door, direction, rest, Led1, Led2, Led3, Led4;
 input wire  Flr1, Flr2, Flr3, Flr4, A,B,Flr1_up, Flr2_up, Flr2_down, Flr3_up, Flr3_down, Flr4_down,reset, clock;
 
 
 
 datapath I2 ( sevseg, segout, clock, reset);
 
controller I1( Door, start_tmr2, start_tmr1, Flr1, Flr2, Flr3, Flr4, Led1, Led2, Led3, Led4,expired2, segout,expired1, clock, reset, direction, rest, A, B, Flr1_up, Flr2_up,
 Flr2_down, Flr3_up, Flr3_down, Flr4_down);
 
 Door_delay_TMR1 TMR1 (clock_out, start_tmr1, expired1);
 
 Elevator_Timer_TMR2 TMR2 (clock_out, start_tmr2, expired2);
 
 clockDivider D2(clock,rst,clock_out);
 
endmodule


module controller ( Door, start_tmr2, start_tmr1, Flr1, Flr2, Flr3, Flr4,, Led1, Led2, Led3, Led4,expired2, segout,expired1, clock, reset, direction, rest, A, B, 
Flr1_up, Flr2_up, Flr2_down, Flr3_up, Flr3_down, Flr4_down);
  
 
  input wire A,B, clock, reset, Flr1_up, Flr2_up, Flr2_down, Flr3_up, expired1,expired2,Flr3_down, Flr4_down, Flr1, Flr2, Flr3, Flr4;
  
  output reg [0:6] segout;
  
  output reg direction, rest, start_tmr2, start_tmr1, Door, Led1, Led2, Led3, Led4;
 
  reg [3:0] CurrentState, NextState;	
																							
  parameter Floor_01_open  = 4'b0010; 
  parameter Floor_01_closed= 4'b0001;
  parameter Floor_02_open  = 4'b0110; 
  parameter Floor_02_closed= 4'b0101;
  parameter Floor_03_open  = 4'b1010; 
  parameter Floor_03_closed= 4'b1001;
  parameter Floor_04_open  = 4'b1110; 
  parameter Floor_04_closed= 4'b1101;
   
/* SET THE PRESENT STATE EQUAL TO THE NEXT STATE */
always @( posedge clock or negedge reset) begin
  if( reset == 0 ) CurrentState <= Floor_01_closed ;
  else CurrentState <= NextState;
  
  end

    
/* DETERMINE THE NEXT STATE */													
  always @ (direction or start_tmr2 or CurrentState or A or B or Flr1_up or Flr2_up or Flr2_down or Flr3_up or Flr3_down or Flr4_down or Flr1 or 
  Flr2 or Flr3 or Flr4) begin
 begin
 
   case( CurrentState )
   Floor_01_open : 	begin if ( Flr1==1) NextState <= Floor_01_open; 
					if (Flr2==1) NextState <= Floor_01_closed ;		
					if( Flr3==1) NextState <= Floor_01_closed;					
					if( Flr4==1) NextState <= Floor_01_closed;
					if ( Flr1_up==1);
					if (Flr2_up==1);
					if ( Flr2_down==1 );
					if( Flr3_up==1);
					if(  Flr3_down==1);
					if( Flr4_down==1);
					if (A==1) start_tmr1 <= 1'b1;
					if (B==1) NextState <= Floor_01_closed;
					else  rest = 1'b1;
					end
					
	 Floor_01_closed : 	begin if ( Flr1==1) NextState <= Floor_01_open; 
					if (Flr2==1) NextState <= Floor_02_closed ; 			start_tmr2 <= 1'b1;   direction =1;
					if( Flr3==1) NextState <= Floor_02_closed;				start_tmr2 <= 1'b1;direction =1;
					if( Flr4==1) NextState <= Floor_02_closed;				start_tmr2 <= 1'b1;direction =1;
					if ( Flr1_up==1) NextState <= Floor_01_open; 			
					if (Flr2_up==1) NextState <= Floor_02_closed ; start_tmr2 <= 1'b1; direction=1;
					if ( Flr2_down==1 ) NextState <= Floor_02_closed ; start_tmr2 <= 1'b1; direction=1;
					if( Flr3_up==1) NextState <= Floor_02_closed; 	start_tmr2 <= 1'b1; direction =1;
					if(  Flr3_down==1) NextState <= Floor_02_closed; 	start_tmr2 <= 1'b1; direction =1;
					if( Flr4_down==1) NextState <= Floor_02_closed;						start_tmr2 <= 1'b1;direction =1;
					if (A==1) ;
					if (B==1) ;
					
					end
					
	 Floor_02_open : 	begin if ( Flr1==1) NextState <= Floor_02_closed;	 
					if( Flr2==1) NextState <= Floor_02_open;	
					if (Flr4==1) NextState <= Floor_02_closed ;				
					if( Flr3==1) NextState <= Floor_02_closed;
					if ( Flr1_up==1);
					if (Flr2_up==1);
					if ( Flr2_down==1 );
					if( Flr3_up==1);
					if(  Flr3_down==1);
					if( Flr4_down==1)	;				
					if (A==1) start_tmr1 <= 1'b1;
					if (B==1) NextState <= Floor_02_closed;
					else  rest = 1'b1;
					end
					
	 Floor_02_closed : 	begin if ( Flr2==1) NextState <= Floor_02_open;	
					if (Flr3==1) NextState <= Floor_03_closed ; 			start_tmr2 <= 1'b1; 		direction =1;
					if( Flr4==1) NextState <= Floor_03_closed;				start_tmr2 <= 1'b1;	direction =1;
					if( Flr1==1) NextState <= Floor_01_closed;				start_tmr2 <= 1'b1;	direction =0;
					if (Flr2_up==1) NextState <= Floor_02_open; 
					if (Flr2_down==1 ) NextState <= Floor_02_open; 
					if (Flr1_up==1 ) NextState <= Floor_01_closed ;						start_tmr2 <= 1'b1;	direction =0;
					if( Flr3_up==1) NextState <= Floor_03_closed; 	start_tmr2 <= 1'b1;	direction =1;
					if(Flr3_down==1) NextState <= Floor_03_closed; 	start_tmr2 <= 1'b1;	direction =1;
					if( Flr4_down==1) NextState <= Floor_03_closed;						start_tmr2 <= 1'b1;	direction =1;
					if (A==1) ;
					if (B==1) ;
					
					
					end
					
	 Floor_03_open : 	begin if( Flr3==1) NextState <= Floor_03_open;	
					if (Flr2==1) NextState <= Floor_03_closed ; 		
					if( Flr4==1) NextState <= Floor_03_closed;			
					if ( Flr1==1) NextState <= Floor_03_closed; 
					if ( Flr1_up==1);
					if (Flr2_up==1);
					if ( Flr2_down==1 );
					if( Flr3_up==1);
					if(  Flr3_down==1);
					if( Flr4_down==1)	;		
					if (A==1) start_tmr1 <= 1'b1;
					if (B==1) NextState <= Floor_03_closed;
					else  rest = 1'b1;
					end
					
	 Floor_03_closed :
					begin if ( Flr3==1) NextState <= Floor_03_open;	
					if (Flr2==1) NextState <= Floor_02_closed ; 			start_tmr2 <= 1'b1;		direction =0;
					if( Flr4==1) NextState <= Floor_04_closed;				start_tmr2 <= 1'b1;	direction =1;
					if( Flr1==1) NextState <= Floor_02_closed;				start_tmr2 <= 1'b1;	direction =0;
					if (Flr3_up==1  ) NextState <= Floor_03_open; 	
					if ( Flr3_down==1 ) NextState <= Floor_03_open; 	
					if (Flr1_up==1 ) NextState <= Floor_02_closed ;						start_tmr2 <= 1'b1;	direction =0;
					if( Flr2_up==1) NextState <= Floor_02_closed; 	start_tmr2 <= 1'b1;	direction =0;
					if(  Flr2_down==1) NextState <= Floor_02_closed; 	start_tmr2 <= 1'b1;	direction =0;
					if( Flr4_down==1) NextState <= Floor_04_closed;						start_tmr2 <= 1'b1;	direction =1;
					if (A==1) ;
					if (B==1) ;
					
					end
					
	 Floor_04_open : 	begin if( Flr4==1) NextState <= Floor_04_open;
					if (Flr3==1) NextState <= Floor_04_closed ; 		
					if( Flr2==1) NextState <= Floor_04_closed;			
					if ( Flr1==1) NextState <= Floor_04_closed; 
					if ( Flr1_up==1);
					if (Flr2_up==1);
					if ( Flr2_down==1 );
					if( Flr3_up==1);
					if(  Flr3_down==1);
					if( Flr4_down==1)	;		
					if (A==1) start_tmr1 <= 1'b1;
					if (B==1) NextState <= Floor_04_closed;
					else  rest = 1'b1;
					end
					
	 Floor_04_closed : 	begin if ( Flr4==1) NextState <= Floor_04_open;
					if (Flr2==1) NextState <= Floor_03_closed ; 			start_tmr2 <= 1'b1;		direction =0;
					if( Flr3==1) NextState <= Floor_03_closed;				start_tmr2 <= 1'b1;	direction =0;
					if( Flr1==1) NextState <= Floor_03_closed;				start_tmr2 <= 1'b1;	direction =0;
					if ( Flr4_down==1) NextState <= Floor_04_open; 			
					if (Flr2_up==1 ) NextState <= Floor_03_closed ; start_tmr2 <= 1'b1;	direction =0;
					if ( Flr2_down==1 ) NextState <= Floor_03_closed ; start_tmr2 <= 1'b1;	direction =0;
					if( Flr3_up==1) NextState <= Floor_03_closed; 	start_tmr2 <= 1'b1;	direction =0;
					if(  Flr3_down==1) NextState <= Floor_03_closed; 	start_tmr2 <= 1'b1;	direction =0;
					if( Flr1_up==1) NextState <= Floor_03_closed;							start_tmr2 <= 1'b1;direction =0;
					if (A==1) ;
					if (B==1) ;
					
					end
					
	
  
	
   default: CurrentState <=Floor_01_closed ;//	NextState <= Floor_01_closed ;
   
   endcase
	
end	
end
   
 



 
   /* ISSUE COMMANDS TO THE DATAPATH */
  always @ (CurrentState or Led1 or Led2 or Led3 or Led4 or Door) begin
	
   case( CurrentState )
	Floor_04_closed :begin Led1 = 0; Led2 = 0; Led3 = 0; Led4 = 1; Door=0; end
	Floor_04_open   :begin Led1 = 0; Led2 = 0; Led3 = 0; Led4 = 1; Door=1; end
	Floor_03_closed :begin Led1 = 0; Led2 = 0; Led3 = 1; Led4 = 0; Door=0; end
	Floor_03_open   :begin Led1 = 0; Led2 = 0; Led3 = 1; Led4 = 0; Door=1; end
	Floor_02_closed :begin Led1 = 0; Led2 = 1; Led3 = 0; Led4 = 0; Door=0; end
	Floor_02_open   :begin Led1 = 0; Led2 = 1; Led3 = 0; Led4 = 0; Door=1; end
	Floor_01_closed :begin Led1 = 1; Led2 = 0; Led3 = 0; Led4 = 0; Door=0; end
	Floor_01_open   :begin Led1 = 1; Led2 = 0; Led3 = 0; Led4 = 0; Door=1; end
	

   default: NextState <= Floor_01_closed ;
				
   endcase
   

	
	end
 
   
 /* TELLS DATAPATH WHAT TO OUTPUT TO THE SEVSEG */
  always @ (CurrentState or segout or direction or rest) begin
 
   if ( direction == 1) segout = 7'b1000001; 
	 if ( direction == 0) segout = 7'b1000010;
	 if ( rest == 1) segout = 7'b1111110;

		 end
		 
endmodule



module datapath (sevseg, segout, clock, reset);
  
  //output reg Led1, Led2, Led3, Led4; 
  output reg [0:6] sevseg;

  input wire  clock, reset;
  
  input wire [0:6] segout;
  
always @ (posedge clock or negedge reset) begin
  if (reset == 0) begin

 sevseg <= segout;
  
  end
  
  else begin
  
  sevseg <= 7'b0000001;
  
 
 
 end
    
  
 end
   
endmodule





module Elevator_Timer_TMR2 (clock, start_tmr2, expired);    //60s delay for elevator

input clock;
input start_tmr2;
output reg expired;
reg [6:0] count=7'b0000000;

always @(posedge clock)

	
	begin
	if (start_tmr2==1)
	begin
		if(count == 6'b111100) //count=60
		begin
			expired <= 1'b1;
			
			count <= 6'b000000;
			
		end
	
		else
		begin 
			expired <= 1'b0;
			count <= count+1;
		end
	end
	else if (start_tmr2==0) 
		begin
		expired <= 1'b0;
		end
	
	end

endmodule

module Door_delay_TMR1 (clock, start_tmr1, expired);  //10s timer for door closing

input clock;
input start_tmr1;
output reg expired;
reg [3:0] count=4'b0000;


always @(posedge clock)

	
	begin
	if (start_tmr1==1)
	begin
		if(count == 4'b1010) //count=10 
		begin
			expired <= 1'b1;
			
			count <= 4'b0000;
			
		end
	
		else
		begin 
			expired <= 1'b0;
			count <= count+1;
		end
	end
	else if (start_tmr1==0) 
		begin
		expired <= 1'b0;
		end
	
	end

endmodule

module clockDivider(clock,rst,clock_out);

input clock , rst;
output reg clock_out;
localparam divider = 1000;
reg [8:0] count;

always @(posedge clock or negedge rst)
begin
	if (!rst)
	begin
		clock_out <= 1'b0;
		count <= 500;
	end
	
	else
	begin
		if(count == 9'b000000001)    //count=1000
		begin
			clock_out <= ~clock_out;
			count <= divider/2;
		end
	
		else
		begin 
			count <= count-1;
		end
	end
end
	
endmodule

// switch Debounce Module
// create a delay of 0.01 sec

module debounce (reset, clock, noisy, clean);
   parameter DELAY = 1000;
   input reset, clock, noisy;
   output clean;

   reg [18:0] count;
   reg new, clean;

   always @(posedge clock)
     if (reset)
       begin
	  count <= 0;
	  new <= noisy;
	  clean <= noisy;
       end
     else if (noisy != new)
       begin
	  new <= noisy;
	  count <= 0;
       end
     else if (count == DELAY)
       clean <= new;
     else
       count <= count+1;
endmodule



module tb_Elevator_Controller_Systems;

 reg Flr1, Flr2, Flr3, Flr4, A,B,Flr1_up, Flr2_up, Flr2_down, Flr3_up, Flr3_down, Flr4_down,reset, clock;
 wire Door, direction, rest, Led1, Led2, Led3, Led4;

wire [0:6] sevseg;

Elevator_Controller_Systems UUT(.clock(clock), .reset(reset),.Flr1(Flr1), .Flr2(Flr2), .Flr3(Flr3), .Flr4(Flr4), .A(A),.B(B),.Flr1_up(Flr1_up),
 .Flr2_up(Flr2_up), .Flr2_down(Flr2_down), .Flr3_up(Flr3_up), .Flr3_down(Flr3_down), .Flr4_down(Flr4_down),.Door(Door), .direction(direction),
 .rest(rest), .Led1(Led1), .Led2(Led2), .Led3(Led3), .Led4(Led4), .sevseg(sevseg));
 
 initial begin
 clock = 0;
 forever #5000 clock = ~clock;
 end 
 initial begin
 
  reset = 1; #40000;
  
	 reset=0; A=0;B=0; Flr1=1; Flr2=0;Flr3=0;Flr4=0; Flr1_up=0;Flr2_down=0;Flr2_up=0;Flr3_down=0;Flr3_up=0;Flr4_down=0; #30000;
  
    reset=0; A=0;B=1; Flr1=0; Flr2=0;Flr3=0;Flr4=0; Flr1_up=0;Flr2_down=0;Flr2_up=0;Flr3_down=0;Flr3_up=0;Flr4_down=0; #30000;
  
    reset=0; A=0;B=0; Flr1=0; Flr2=1;Flr3=0;Flr4=1; Flr1_up=0;Flr2_down=0;Flr2_up=0;Flr3_down=0;Flr3_up=0;Flr4_down=0; #30000;
  
    reset=0; A=0;B=0; Flr1=0; Flr2=0;Flr3=1;Flr4=0; Flr1_up=0;Flr2_down=0;Flr2_up=0;Flr3_down=0;Flr3_up=0;Flr4_down=1; #30000;
  
    reset=0; A=0;B=0; Flr1=0; Flr2=1;Flr3=0;Flr4=0; Flr1_up=0;Flr2_down=0;Flr2_up=1;Flr3_down=0;Flr3_up=0;Flr4_down=0; #30000;
  
    reset=0; A=0;B=0; Flr1=0; Flr2=0;Flr3=1;Flr4=0; Flr1_up=0;Flr2_down=0;Flr2_up=0;Flr3_down=0;Flr3_up=0;Flr4_down=0; #30000;
  
    reset=0; A=1;B=0; Flr1=0; Flr2=0;Flr3=0;Flr4=1; Flr1_up=0;Flr2_down=0;Flr2_up=0;Flr3_down=0;Flr3_up=0;Flr4_down=0; #30000;
	 
	 reset=0; A=0;B=0; Flr1=0; Flr2=0;Flr3=0;Flr4=0; Flr1_up=1;Flr2_down=0;Flr2_up=0;Flr3_down=1;Flr3_up=0;Flr4_down=0; #30000;
  
    reset=0; A=0;B=0; Flr1=0; Flr2=0;Flr3=0;Flr4=1; Flr1_up=0;Flr2_down=0;Flr2_up=0;Flr3_down=0;Flr3_up=0;Flr4_down=0; #30000;
  
    reset=0; A=1;B=0; Flr1=0; Flr2=0;Flr3=0;Flr4=0; Flr1_up=0;Flr2_down=0;Flr2_up=0;Flr3_down=0;Flr3_up=0;Flr4_down=0; #30000;
  
    reset=0; A=0;B=0; Flr1=0; Flr2=0;Flr3=0;Flr4=0; Flr1_up=0;Flr2_down=0;Flr2_up=0;Flr3_down=1;Flr3_up=0;Flr4_down=0; #30000;
  
    reset=0; A=0;B=0; Flr1=0; Flr2=0;Flr3=0;Flr4=0; Flr1_up=0;Flr2_down=0;Flr2_up=0;Flr3_down=0;Flr3_up=0;Flr4_down=1; #30000;
  
    reset=0; A=0;B=0; Flr1=0; Flr2=0;Flr3=1;Flr4=0; Flr1_up=0;Flr2_down=0;Flr2_up=0;Flr3_down=0;Flr3_up=0;Flr4_down=0; #30000;
  
    reset=0; A=0;B=1; Flr1=0; Flr2=0;Flr3=0;Flr4=0; Flr1_up=0;Flr2_down=0;Flr2_up=0;Flr3_down=0;Flr3_up=0;Flr4_down=0; #30000;
  
    reset=0; A=0;B=0; Flr1=0; Flr2=0;Flr3=0;Flr4=0; Flr1_up=1;Flr2_down=0;Flr2_up=0;Flr3_down=0;Flr3_up=0;Flr4_down=0; #30000;
  
    reset=0; A=1;B=0; Flr1=0; Flr2=0;Flr3=0;Flr4=0; Flr1_up=0;Flr2_down=0;Flr2_up=0;Flr3_down=0;Flr3_up=0;Flr4_down=0; #30000;
  
    reset=0; A=0;B=1; Flr1=0; Flr2=0;Flr3=0;Flr4=0; Flr1_up=0;Flr2_down=0;Flr2_up=0;Flr3_down=0;Flr3_up=0;Flr4_down=0; #30000;
  
    reset=0; A=1;B=0; Flr1=0; Flr2=0;Flr3=0;Flr4=0; Flr1_up=0;Flr2_down=0;Flr2_up=0;Flr3_down=0;Flr3_up=0;Flr4_down=0; #30000;
  
    reset=0; A=0;B=1; Flr1=0; Flr2=0;Flr3=0;Flr4=0; Flr1_up=0;Flr2_down=1;Flr2_up=0;Flr3_down=0;Flr3_up=0;Flr4_down=0; #30000;
	 
	 reset=0; A=0;B=0; Flr1=0; Flr2=0;Flr3=0;Flr4=0; Flr1_up=0;Flr2_down=0;Flr2_up=0;Flr3_down=0;Flr3_up=0;Flr4_down=0; #30000;
  
    reset=0; A=0;B=0; Flr1=0; Flr2=0;Flr3=0;Flr4=0; Flr1_up=0;Flr2_down=0;Flr2_up=0;Flr3_down=0;Flr3_up=0;Flr4_down=1; #30000;
  
    reset=0; A=0;B=0; Flr1=0; Flr2=0;Flr3=0;Flr4=0; Flr1_up=1;Flr2_down=0;Flr2_up=0;Flr3_down=0;Flr3_up=0;Flr4_down=0; #30000;
  
    reset=0; A=0;B=0; Flr1=0; Flr2=0;Flr3=0;Flr4=0; Flr1_up=0;Flr2_down=0;Flr2_up=1;Flr3_down=0;Flr3_up=0;Flr4_down=0; #30000;
  
    reset=0; A=0;B=1; Flr1=0; Flr2=0;Flr3=0;Flr4=0; Flr1_up=0;Flr2_down=0;Flr2_up=0;Flr3_down=0;Flr3_up=0;Flr4_down=0; #30000;
    
 
 end
      
endmodule