#![feature(const_type_name)]
#![feature(once_cell)]

extern crate hadron;
use hadron::app::App;

pub mod ai;
pub mod sim;
pub mod sector;
pub mod grid;


/**
 * Options:
 *      - Run game, engine as library << winner
 * 
 * Program load:
 *      - Run game
 *      - Load graphics configs
 *      - Setup window/graphics
 *      - Configure graphics stuff
 *      - Get a minimal GUI up to show loading errors
 *      - Load engine configs
 *      - Load game configs
 *      - Load resources using prev config data
 *      - Configure engine stuff
 *      - Reconfigure window to proper dimensions
 *      - Start and run game
 * 
 */
fn main() -> Result<(), Box<dyn std::error::Error>> {
    std::env::set_var("RUST_BACKTRACE", "1"); // FOR DEBUG

    let app = App::new().expect("Couldn't create app");
    app.run();

    Ok(())
}
