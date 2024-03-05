use pai_macros::PaiRegs;
#[derive(Default, PaiRegs)]
struct Regs {
	#[sp]
	abcd: usize,
}
#[test]
fn test() {
	let regs = Regs::default();
	assert_eq!(regs._offset_of("abcd").unwrap(), 0);
}
