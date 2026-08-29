use crate::display::Display;
use crate::ppu::{SCALE, XRES, YRES};
use sdl2::pixels::Color;
use sdl2::render::WindowCanvas;

pub struct SdlDisplay {
    pub is_enabled: bool,
    canvas: WindowCanvas,
    // pos: usize,
    buf: [u8; 160 * 144],
}

impl SdlDisplay {
    pub fn new(canvas: WindowCanvas) -> SdlDisplay {
        SdlDisplay {
            is_enabled: false,
            canvas,
            // pos: 0,
            buf: [0; 160 * 144],
        }
    }
}

impl Display for SdlDisplay {
    fn enable(&mut self) {
        self.is_enabled = true;
    }

    fn enabled(&self) -> bool {
        self.is_enabled
    }

    fn disable(&mut self) {
        self.is_enabled = false;
    }

    fn write(&mut self, x: u8, y: u8, color: u8) {
        if x as usize >= XRES || y as usize >= YRES {
            return;
        }
        self.buf[y as usize * XRES + x as usize] = color;
    }

    fn hblank(&mut self) {}

    fn vblank(&mut self) {
        // self.pos = 0;
    }

    fn draw(&mut self) {
        // if !self.is_enabled {
        //     return;
        // }
        self.canvas.clear();
        for y in 0..YRES {
            for x in 0..XRES {
                let color = self.buf[y * XRES + x];
                self.canvas.set_draw_color(match color {
                    0 => Color::RGB(255, 255, 255), // White
                    1 => Color::RGB(192, 192, 192), // Light Gray
                    2 => Color::RGB(96, 96, 96),    // Dark Gray
                    3 => Color::RGB(0, 0, 0),       // Black
                    _ => Color::RGB(255, 0, 0),     // Red for error
                });
                let _ = self.canvas.fill_rect(sdl2::rect::Rect::new(
                    (x as u32 * SCALE as u32) as i32,
                    (y as u32 * SCALE as u32) as i32,
                    SCALE as u32,
                    SCALE as u32,
                ));
            }
        }
        // println!("Drawing frame");
        // clear buf
        self.canvas.present();
        // self.pos = 0;
    }
}
