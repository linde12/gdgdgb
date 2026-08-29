pub trait Display {
    /// Enable turns the display on. By default, nothing is displayed.
    fn enable(&mut self);

    /// Enabled returns whether the display is on.
    fn enabled(&self) -> bool;

    /// Disable turns the display off. Should only be called during VBlank.
    fn disable(&mut self);

    /// Write outputs a pixel (defined as a color number) to the display.
    fn write(&mut self, x: u8, y: u8, color: u8);

    /// HBlank is called whenever all pixels in a scanline have been output.
    fn hblank(&mut self);

    /// VBlank is called whenever a full frame has been output.
    fn vblank(&mut self);

    fn draw(&mut self);
}
