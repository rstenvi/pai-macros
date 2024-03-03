use proc_macro::TokenStream;
use quote::quote;

struct Code;

impl Code {
	fn generate(stream: TokenStream) -> TokenStream {
		// Names for first and second argument
		const CTX: &str = "ctx";
		const FRAME: &str = "frame";
		
		// Parse as a function
		let input = syn::parse_macro_input!(stream as syn::ItemFn);
		let mut insns = Vec::new();

		// Extract different parts
		let syn::ItemFn { attrs, vis, mut sig, block } = input;
		let funcident = sig.ident;
		let funcout = sig.output;

		// The initial idents we keep in signature unchanged Need to store a
		// reference to them so that we can refer to them when creating new
		// variables.
		let mut idents = Vec::new();

		// let mut sep: usize = 2;
		let inputs = std::mem::take(&mut sig.inputs);
		let mut inputs = inputs.into_iter().collect::<Vec<syn::FnArg>>();

		// This part is a bit messy, but we want to support all possible cases of
		// including or omitting `ctx` or `frame`. In addition, the user may or may
		// not have specified extra arguments.
		if let Some(ctx) = inputs.first() {
			if Self::is_ptr_to(ctx, "Secondary") {
				// fn(&mut Context, ???)
				let ctx = inputs.remove(0);
				let ident = Self::get_ident(&ctx);
				idents.push(ident);
			} else {
				// fn(???)
				// Don't know what was supplied, try parsing it as CallFrame below
				idents.push(syn::Ident::new(CTX, proc_macro2::Span::call_site()));
			};
			
			if let Some(frame) = inputs.first() {
				if Self::is_ptr_to(&frame, "CallFrame") {
					// fn(???, &CallFrame, arg...)
					let frame = inputs.remove(0);
					let ident = Self::get_ident(&frame);
					idents.push(ident);
				} else {
					// fn(???, arg...)
					idents.push(syn::Ident::new(FRAME, proc_macro2::Span::call_site()));
				}
			} else {
				// fn(&mut Context)
				// No arguments after Context
				idents.push(syn::Ident::new(FRAME, proc_macro2::Span::call_site()));
			}
		} else {
			// fn()
			// No arguments is supplied at all
			idents.push(syn::Ident::new(CTX, proc_macro2::Span::call_site()));
			idents.push(syn::Ident::new(FRAME, proc_macro2::Span::call_site()));
		}

		// Start parsing the arguments the user wants parsed.
		let ctx = idents.first().expect("no context object");
		let frame = idents.get(1).expect("no frame object");
		for (i, input) in inputs.into_iter().enumerate() {
			let ident = Self::get_ident(&input);
			if let syn::FnArg::Typed(syn::PatType {ty, .. }) = input {
				let ty = *(ty).clone();
				let ins = Self::parse_type(ty, &ident, frame, ctx, i, false);
				insns.push(ins);
			} else {
				panic!("unable to parse argument with ident {ident:?}");
			}
		}

		let stmts = &block.stmts;
		let v = quote! {
			#(#attrs)* #vis fn #funcident<T> (#ctx: &mut pai::ctx::Secondary<T, pai::Error>, #frame: &pai::api::CallFrame) #funcout {
				#(#insns)*
				#(#stmts)*
			}
		};
		v.into()
	}



	fn is_ptr_to(arg: &syn::FnArg, check: &str) -> bool {
		if let syn::FnArg::Typed(syn::PatType {ty, .. }) = arg {
			let ty = *(ty).clone();
			if let syn::Type::Reference(p) = ty {
				let ty = *(p.elem).clone();
				
				if let syn::Type::Path(p) = ty {
					if p.qself.is_none() && Self::path_is_matching(&p.path, check) {
						println!("matched generic");
						true
					} else {
						println!("{:?}", p.path.get_ident());
						let ret = p.path.is_ident(check);
						println!("match = {ret}");
						ret
					}
				} else {
					false
				}
			} else {
				false
			}
		} else {
			false
		}
	}


	fn path_is_option(path: &syn::Path) -> bool {
		path.leading_colon.is_none()
			&& path.segments.len() == 1
			&& path.segments.iter().next().unwrap().ident == "Option"
	}
	fn path_is_matching(path: &syn::Path, check: &str) -> bool {
		path.leading_colon.is_none()
			&& path.segments.len() == 1
			&& path.segments.iter().next().unwrap().ident == check
	}
	
	fn parse_type(ty: syn::Type, ident: &proc_macro2::Ident, frame: &proc_macro2::Ident, ctx: &proc_macro2::Ident, argnum: usize, inopt: bool) -> proc_macro2::TokenStream {
		let framearg = quote! { #frame.arg(#argnum, #ctx.client_mut())? };
		if let syn::Type::Path(p) = ty {
			if p.path.is_ident("i64") {
				assert!(!inopt);
				quote! { let #ident = #framearg.as_i64(); }
			} else if p.path.is_ident("i32") {
				assert!(!inopt);
				quote! { let #ident = #framearg.as_i32(); }
			} else if p.path.is_ident("i16") {
				assert!(!inopt);
				quote! { let #ident = #framearg.as_i16(); }
			} else if p.path.is_ident("i8") {
				assert!(!inopt);
				quote! { let #ident = #framearg.as_i8(); }
			} else if p.path.is_ident("isize") {
				assert!(!inopt);
				quote! { let #ident = #framearg.as_isize(); }
			} else if p.path.is_ident("u64") {
				assert!(!inopt);
				quote! { let #ident = #framearg.as_u64(); }
			} else if p.path.is_ident("u32") {
				assert!(!inopt);
				quote! { let #ident = #framearg.as_u32(); }
			} else if p.path.is_ident("u16") {
				assert!(!inopt);
				quote! { let #ident = #framearg.as_u16(); }
			} else if p.path.is_ident("u8") {
				assert!(!inopt);
				quote! { let #ident = #framearg.as_u8(); }
			} else if p.path.is_ident("usize") {
				assert!(!inopt);
				quote! { let #ident = #framearg.as_usize(); }
			} else if p.path.is_ident("String") {
				if !inopt {
					quote! { let #ident = #framearg.read_ptr_as_str(#ctx.client_mut())?; }
				} else {
					quote! { let #ident: Option<String> = #framearg.read_ptr_as_str(#ctx.client_mut()).ok(); }
				}
			} else if p.qself.is_none() && Self::path_is_option(&p.path) {
				assert!(!inopt);
				let type_params = &p.path.segments.first().unwrap().arguments;
				// It should have only on angle-bracketed param ("<String>"):
				let generic_arg = match type_params {
					syn::PathArguments::AngleBracketed(params) => params.args.first().unwrap(),
					_ => panic!("unable to parse {ident:?}"),
				};
				match generic_arg {
					syn::GenericArgument::Type(ty) => {
						Self::parse_type(ty.clone(), ident, frame, ctx, argnum, true)
					},
					_ => panic!("unable to parse {ident:?}"),
				}
			} else {
				panic!("custom idents not supported yet");
				// let custom = p.path.get_ident().expect("unable to get ident for {ident:?}");
				// if !inopt {
				// 	quote! { let #ident = #framearg.to_struct::<#custom>(#ctx.client_mut())?; }
				// } else {
				// 	quote! { let #ident: Option<#custom> = #framearg.to_struct::<#custom>(#ctx.client_mut()).ok(); }
				// }
			}
		} else {
			panic!("expected Type::Path");
		}
	}
	
	fn get_ident(arg: &syn::FnArg) -> syn::Ident {
		if let syn::FnArg::Typed(syn::PatType {ty: _, pat, .. }) = arg {
			let pat = *(pat).clone();
			if let syn::Pat::Ident(id) = pat {
				return id.ident.clone();
			}
		}
		panic!("unable to find ident");
	}
}

#[proc_macro_attribute]
pub fn pai_hook(_attr: TokenStream, stream: TokenStream) -> TokenStream {
	Code::generate(stream)
}

struct RegsCode;

impl RegsCode {
	fn generate(stream: TokenStream) -> TokenStream {
		let input = syn::parse_macro_input!(stream as syn::DeriveInput);
		let name = input.ident.clone();
		let syn::Data::Struct(input) = input.data else { panic!("") };
		let mut gets = Vec::new();
		let mut sizes = Vec::new();
		let mut sp: Option<(syn::Ident, syn::Type)> = None;
		let mut pc: Option<(syn::Ident, syn::Type)> = None;
		let mut set_sysno: Option<(syn::Ident, syn::Type)> = None;
		let mut get_sysno: Option<syn::Ident> = None;
		let mut fields = Vec::new();
		for field in input.fields.iter() {
			let ident = field.ident.as_ref().unwrap().clone();

			for attr in field.attrs.iter() {
				let p = attr.path();
				if p.is_ident("sp") {
					sp = Some((ident.clone(), field.ty.clone()));
				} else if p.is_ident("pc") {
					pc = Some((ident.clone(), field.ty.clone()));
				} else if p.is_ident("sysno") {
					set_sysno = Some((ident.clone(), field.ty.clone()));
					get_sysno = Some(ident.clone());
				} else if p.is_ident("setsysno") {
					set_sysno = Some((ident.clone(), field.ty.clone()));
				} else if p.is_ident("getsysno") {
					get_sysno = Some(ident.clone());
				}
			}

			let name = format!("{ident}");
			fields.push(name.clone());
			let ins = quote! { #name => Some(std::mem::offset_of!(Self, #ident)), };
			gets.push(ins);
			let ins = quote! { #name => Some(std::mem::size_of_val(&self.#ident)), };
			sizes.push(ins);
		}
		let spcode = if let Some((sp, ty)) = sp {
			quote! {
				fn _get_sp(&self) -> u64 {
					self.#sp as u64
				}
				fn _set_sp(&mut self, sp: u64) {
					self.#sp = sp as #ty;
				}
			}
		} else {
			quote!()
		};
		let pccode = if let Some((pc, ty)) = pc {
			quote! {
				fn _get_pc(&self) -> u64 {
					self.#pc as u64
				}
				fn _set_pc(&mut self, pc: u64) {
					self.#pc = pc as #ty;
				}
			}
		} else {
			quote!()
		};
		let sysnocode = if let Some(get) = get_sysno {
			let (set, ty) = set_sysno.unwrap();
			quote! {
				fn _get_sysno(&self) -> usize {
					self.#get as usize
				}
				fn _set_sysno(&mut self, sysno: usize) {
					self.#set = sysno as #ty;
				}
			}
		} else {
			quote!()
		};
		let res = quote! {
			impl #name {
				fn _offset_of(&self, regs: &str) -> Option<usize> {
					match regs {
						#(#gets)*
						_ => None,
					}
				}
				fn _size_of(&self, regs: &str) -> Option<usize> {
					match regs {
						#(#sizes)*
						_ => None,
					}
				}
				fn _fields(&self) -> &[&str] {
					&[#(#fields),*]
				}
				unsafe fn _get_value(&self, offset: usize, size: usize, data: &mut Vec<u8>) {
					let v: *const u8 = unsafe { std::mem::transmute(self) };
					let v = unsafe { v.byte_add(offset) };
					let v = unsafe { std::slice::from_raw_parts(v, size) };
					data.extend_from_slice(v);
				}
				unsafe fn _set_value(&mut self, offset: usize, data: &[u8]) {
					let v: *mut u8 = unsafe { std::mem::transmute(self) };
					let v = unsafe { v.byte_add(offset) };
					let v: &mut [u8] = unsafe { std::slice::from_raw_parts_mut(v, data.len()) };
					for (i, b) in v.iter_mut().enumerate() {
						*b = data[i];
					}
				}
				#spcode
				#pccode
				#sysnocode
			}
		};
		res.into()
	}

}

#[proc_macro_derive(PaiRegs, attributes(sp, pc, sysno, getsysno, setsysno))]
pub fn derive_regs_attr(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
	RegsCode::generate(input)
}