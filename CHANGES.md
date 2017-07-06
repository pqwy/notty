## v0.2.0 (trunk):

Breaking changes:
* Switch over to `Uchar.t`
* Rename input `#key` to `#special`
* Separate ASCII from general Unicode input
* Move the cursor origin from (1, 1) to (0, 0)

Other changes:
* More examples
* Reduced memory pressure
* Lines are now completely redrawn; reduces blinking
* Option to inhibit synthetic TTY signals on term creation
* Query a terminal for file descriptors it is connected to
* Add attr and image equality
* Import a private copy of width-related Unicode 8.0.0 properties

## v0.1.1 (2016-02-09):
* `Term.input` -> `Term.event`
* Option to redraw the line

## v0.1.0 (2016-02-09):
* Initial release
