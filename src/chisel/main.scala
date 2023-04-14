import chisel3._
import chisel3.util._

object Conf{
    val numLeds = 24 // Number of LEDs
    val fpgaFrequency = 100000000 // Mhz
    val ledShiftDuration = (0.1 * fpgaFrequency).toInt  // The duration between a shift of leds
}

object Util{
    def detectRisingEdge(x: Bool) = x && !RegNext(x)
    def detectFallingEdge(x: Bool) = !x && RegNext(x)
}

class UpCounter(maxCount: Int) extends Module{
    val io = IO(new Bundle{
        val enable = Input(UInt(1.W))
        val count = Output(UInt(log2Ceil(maxCount).W))
    })
    val UpCounter = RegInit(0.U(log2Ceil(maxCount).W))
    when(io.enable.asBool){
        UpCounter := Mux(UpCounter===(maxCount-1).U, 0.U, UpCounter + 1.U)
    }
    io.count := UpCounter
}

class UnsignedComparator(width: Int) extends Module {
    val io = IO(new Bundle{
        val input1 = Input(UInt(width.W))
        val input2 = Input(UInt(width.W))
        val output = Output(UInt(1.W))
    })
    io.output:= io.input1 < io.input2
}

class PulseGenerator(pulseThreshold:Int) extends Module {
    val io = IO(new Bundle{
        val pulse = Output(UInt(1.W))
    })
    // Free UpCounter
    val pulseCounter = Module(new UpCounter(pulseThreshold))
    pulseCounter.io.enable := 1.U // will count without stopping
    
    // compare the pulseCounter.count with the pulseThreshold
    val pulse_comparator = Module(new UnsignedComparator(log2Ceil(pulseThreshold)))
    pulse_comparator.io.input2 := (pulseThreshold-1).U
    pulse_comparator.io.input1 := pulseCounter.io.count

    // Detect the cycle when the UpCounter is equal to pulseThreshold
    io.pulse := Util.detectFallingEdge(pulse_comparator.io.output.asBool)
}

class ledBrightnessPwm(numLeds: Int) extends Module {
    val io = IO(new Bundle{
        val ledsBrightness = Output(Vec(numLeds, UInt(1.W)))
    })

    val ledBrightnessCounter = Module(new UpCounter(numLeds))
    ledBrightnessCounter.io.enable := 1.U
    
    val ledBrightnessThresholds = List.fill(numLeds)(Module(new UnsignedComparator(log2Ceil(numLeds))))

    for(led <- 0 until numLeds){
        ledBrightnessThresholds(led).io.input1 := ledBrightnessCounter.io.count
        ledBrightnessThresholds(led).io.input2 := led.U
        io.ledsBrightness(led) := ledBrightnessThresholds(led).io.output
    }
}

class LedShifter(numLeds:Int, ledShiftDuration:Int) extends Module{
    val io = IO(new Bundle{
        val ledsBrightness = Input(Vec(numLeds, UInt(1.W)))
        val associotedLeds = Output(Vec(numLeds, UInt(1.W)))
    })
    // pulse at the speed of ledShiftDuration
    val shiftPulse = Module(new PulseGenerator(ledShiftDuration))

    // 24 led = 24 position shift = 1 UpCounter
    val ledShiftCounter = Module(new UpCounter(numLeds))
    // the UpCounter increment by one when the pulse occur = led shift occur when pulse 
    ledShiftCounter.io.enable := shiftPulse.io.pulse

    // 24 muxes(for 24 LEDs) of 24 inputs (for the 24 levels of brightness)
    // Mux number N have the same inputs than Mux number N-1 shifted by 1.
    // all Muxes share the same selector : ledShiftCounter.io.count
    // so for every ledShiftCounter.io.count the 24 muxes output a different level of brightness
    for(led <- 0 until numLeds){
        val cases = List.tabulate(numLeds)(idx => (idx.U -> io.ledsBrightness((idx+led)%numLeds))).toSeq
        io.associotedLeds(led) := MuxLookup(ledShiftCounter.io.count, 0.U(1.W),cases)
    }
}

class LedChaser(numLeds: Int, ledShiftDuration: Int) extends Module{
    val io = IO(new Bundle{
        val LEDs = Output(Vec(numLeds, UInt(1.W)))
    })

    val ledsBrightnessModule = Module(new ledBrightnessPwm(numLeds))
    val ledShifterModule     = Module(new LedShifter(numLeds, ledShiftDuration))
   
    for(led <- 0 until numLeds){
        ledShifterModule.io.ledsBrightness(led) := ledsBrightnessModule.io.ledsBrightness(led)
        io.LEDs(led) := ledShifterModule.io.associotedLeds(led)
    }
}

class AlchitryCUTop extends Module {
    val io = IO(new Bundle{
        val ledPins = Output(Vec(Conf.numLeds, UInt(1.W)))
    })
    // the alchitry CU board has an active low reset
    val reset_n = !reset.asBool

    withReset(reset_n){
        val my_led_chaser  = Module(new LedChaser(Conf.numLeds, Conf.ledShiftDuration))
        io.ledPins := my_led_chaser.io.LEDs
    }
}

object Main extends App{
    (new chisel3.stage.ChiselStage).emitVerilog(new AlchitryCUTop, Array("--target-dir", "build/artifacts/netlist/"))
}