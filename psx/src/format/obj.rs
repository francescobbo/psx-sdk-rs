//! Wavefront OBJ format importer
#![allow(missing_docs)]

use crate::math::f16;
use core::mem::MaybeUninit;

// TODO: This module is incredibly unidiomatic rust to ensure most things can be
// const to embed the minimum amount of data necessary in executables. As more
// things become const (particularly slice and &str methods) I should rewrite
// things in a more sane way.

/// Parse an `u16` from a byte slice starting at `idx`.
#[doc(hidden)]
pub const fn parse_u16(data: &[u8], idx: &mut usize) -> u16 {
    let mut res = 0;
    while data[*idx] >= b'0' && data[*idx] <= b'9' {
        res *= 10;
        res += (data[*idx] - b'0') as u16;
        *idx += 1;
    }
    if *idx < data.len() {
        *idx += 1;
    }
    res
}

/// Parse an `f16` from a byte slice starting at `idx`.
#[doc(hidden)]
pub const fn parse_f16(data: &[u8], idx: &mut usize) -> f16 {
    let neg = data[*idx] == b'-';
    if neg {
        *idx += 1;
    }
    let abs_int = (data[*idx] - b'0') as u16;
    assert!(abs_int < 2u16.pow(f16::INT as u32));
    *idx += 1;
    assert!(data[*idx] == b'.');
    *idx += 1;
    let mut frac = 0;
    let mut digits = 0;
    while data[*idx] != b' ' && data[*idx] != b'\n' {
        frac *= 10;
        frac += (data[*idx] - b'0') as u64;
        digits += 1;
        *idx += 1;
    }
    *idx += 1;
    let abs_frac = (frac * 2u64.pow(f16::FRAC as u32) / 10u64.pow(digits)) as u16;
    let abs_fixed = (abs_int << f16::FRAC) | abs_frac;
    let fixed = if neg {
        -(abs_fixed as i16)
    } else {
        abs_fixed as i16
    };
    f16(fixed)
}

/// Count the number of u16s in a face
#[doc(hidden)]
pub const fn count_u16(data: &[u8], offset: usize) -> usize {
    let mut i = offset;
    assert!(data[i] == b'f');
    assert!(data[i + 1] == b' ');
    let mut count = 0;
    while i < data.len() && data[i] != b'\n' {
        if data[i] == b' ' {
            count += 1;
        }
        i += 1;
    }
    count
}

#[doc(hidden)]
pub struct NumFaces {
    pub quads: usize,
    pub tris: usize,
}

/// Count the number of lines starting with `f`.
#[doc(hidden)]
pub const fn count_faces(data: &[u8]) -> NumFaces {
    let mut quads = 0;
    let mut tris = 0;
    let mut i = 0;
    while i < data.len() {
        if data[i] == b'f' && data[i + 1] == b' ' {
            match count_u16(data, i) {
                3 => tris += 1,
                4 => quads += 1,
                _ => panic!("Only tris and quads are currently supported"),
            }
            i += 1;
        } else {
            while i < data.len() && data[i] != b'\n' {
                i += 1;
            }
            if i < data.len() && data[i] == b'\n' {
                i += 1;
            }
        }
    }
    NumFaces { quads, tris }
}

/// Count the number of lines starting with `v`.
#[doc(hidden)]
pub const fn count_vertices(data: &[u8]) -> usize {
    let mut i = 0;
    let mut count = 0;
    while i < data.len() {
        if data[i] == b'v' && data[i + 1] == b' ' {
            count += 1;
        }
        while data[i] != b'\n' {
            i += 1;
        }
        if data[i] == b'\n' {
            i += 1;
        }
    }
    count
}

/// Count the number of lines starting with `vn`.
#[doc(hidden)]
pub const fn count_normals(data: &[u8]) -> usize {
    let mut i = 0;
    let mut count = 0;
    while i < data.len() {
        if data[i] == b'v' && data[i + 1] == b'n' && data[i + 2] == b' ' {
            count += 1;
        }
        while data[i] != b'\n' {
            i += 1;
        }
        if data[i] == b'\n' {
            i += 1;
        }
    }
    count
}

#[derive(Debug)]
#[allow(missing_docs)]
/// A reference to a Wavefront OBJ file.
pub struct Obj<
    'a,
    const VERTICES: usize,
    const NORMALS: usize,
    const QUADS: usize,
    const TRIS: usize,
    const FACES: usize,
> {
    pub quads: &'a mut [[u16; 4]; QUADS],
    pub tris: &'a mut [[u16; 3]; TRIS],

    pub quad_norms: &'a mut [u16; QUADS],
    pub tri_norms: &'a mut [u16; TRIS],
    pub vertices: &'a mut [[f16; 3]; VERTICES],
    pub normals: &'a mut [[f16; 3]; NORMALS],
}

#[derive(Debug)]
pub struct ObjRef<'a> {
    pub quads: &'a [[u16; 4]],
    pub tris: &'a [[u16; 3]],
    pub quad_norms: &'a [u16],
    pub tri_norms: &'a [u16],
    pub vertices: &'a [[f16; 3]],
    pub normals: &'a [[f16; 3]],
}

impl<
        'a,
        const VERTICES: usize,
        const NORMALS: usize,
        const QUADS: usize,
        const TRIS: usize,
        const FACES: usize,
    > Obj<'a, VERTICES, NORMALS, QUADS, TRIS, FACES>
{
    /// Creates an array by calling `f` for each face.
    pub fn for_each_face<T, F>(&self, mut f: F) -> [T; FACES]
    where F: FnMut() -> T {
        let mut res = [const { MaybeUninit::<T>::uninit() }; FACES];
        for n in 0..QUADS + TRIS {
            res[n].write(f());
        }
        unsafe { MaybeUninit::array_assume_init(res) }
    }

    /// Creates an array by applying `f_quad` to each quad and `f_tri` to each
    /// tri.
    pub fn map_faces<T, F, G>(&self, mut f_quad: F, mut f_tri: G) -> [T; FACES]
    where
        F: FnMut([u16; 4]) -> T,
        G: FnMut([u16; 3]) -> T, {
        let mut res = [const { MaybeUninit::<T>::uninit() }; FACES];
        for n in 0..QUADS + TRIS {
            if n < QUADS {
                res[n].write(f_quad(self.quads[n]));
            } else {
                res[n].write(f_tri(self.tris[n - QUADS]));
            }
        }
        unsafe { MaybeUninit::array_assume_init(res) }
    }

    pub fn as_ref(&self) -> ObjRef {
        ObjRef {
            quads: self.quads,
            tris: self.tris,
            quad_norms: self.quad_norms,
            tri_norms: self.tri_norms,
            vertices: self.vertices,
            normals: self.normals,
        }
    }
}

/// Includes the vertices and faces in a Wavefront OBJ file as
/// [`Obj`][`crate::format::obj::Obj`].
///
/// Currently only supports vertices and faces.
#[macro_export]
macro_rules! include_obj {
    ($file:literal) => {{
        use $crate::format::obj::{count_faces, count_normals, count_u16, count_vertices,
                                  parse_f16, parse_u16, NumFaces, Obj};
        use $crate::math::f16;

        const NUM_VERTICES: usize = count_vertices(include_bytes!($file));
        const NUM_NORMALS: usize = count_normals(include_bytes!($file));
        static mut VERTICES: [[f16; 3]; NUM_VERTICES] = {
            let mut vertices = [[f16(0); 3]; NUM_VERTICES];
            let mut n = 0;
            let mut i = 0;
            let obj = include_bytes!($file);
            while i < obj.len() {
                if obj[i] == b'v' && obj[i + 1] == b' ' {
                    i += 2;
                    let x = parse_f16(obj, &mut i);
                    let y = parse_f16(obj, &mut i);
                    let z = parse_f16(obj, &mut i);
                    vertices[n] = [x, y, z];
                    n += 1;
                } else {
                    while i < obj.len() && obj[i] != b'\n' {
                        i += 1;
                    }
                    if i == obj.len() {
                        break
                    }
                    if obj[i] == b'\n' {
                        i += 1;
                    }
                }
            }
            vertices
        };
        static mut NORMALS: [[f16; 3]; NUM_NORMALS] = {
            let mut vertices = [[f16(0); 3]; NUM_NORMALS];
            let mut n = 0;
            let mut i = 0;
            let obj = include_bytes!($file);
            while i < obj.len() {
                if obj[i] == b'v' && obj[i + 1] == b'n' && obj[i + 2] == b' ' {
                    i += 3;
                    let x = parse_f16(obj, &mut i);
                    let y = parse_f16(obj, &mut i);
                    let z = parse_f16(obj, &mut i);
                    vertices[n] = [x, y, z];
                    n += 1;
                } else {
                    while i < obj.len() && obj[i] != b'\n' {
                        i += 1;
                    }
                    if i == obj.len() {
                        break
                    }
                    if obj[i] == b'\n' {
                        i += 1;
                    }
                }
            }
            vertices
        };
        const FACE_COUNT: NumFaces = count_faces(include_bytes!($file));
        const NUM_QUADS: usize = FACE_COUNT.quads;
        const NUM_TRIS: usize = FACE_COUNT.tris;
        const NUM_FACES: usize = NUM_QUADS + NUM_TRIS;
        /// The face indices in a Wavefront OBJ file.
        pub struct Faces<const QUADS: usize, const TRIS: usize> {
            pub quads: [[u16; 4]; QUADS],
            pub tris: [[u16; 3]; TRIS],

            pub quad_norms: [u16; QUADS],
            pub tri_norms: [u16; TRIS],
        }

        static mut FACES: Faces<NUM_QUADS, NUM_TRIS> = {
            let mut quads = [[0; 4]; NUM_QUADS];
            let mut tris = [[0; 3]; NUM_TRIS];
            let mut quad_norms = [0; NUM_QUADS];
            let mut tri_norms = [0; NUM_TRIS];
            let mut n = 0;
            let mut m = 0;
            let mut i = 0;
            let obj = include_bytes!($file);
            while i < obj.len() {
                if obj[i] == b'f' && obj[i + 1] == b' ' {
                    if count_u16(obj, i) == 4 {
                        i += 2;
                        let a = parse_u16(obj, &mut i);
                        while obj[i] != b'/' {
                            i += 1;
                        }
                        while obj[i] != b'/' {
                            i += 1;
                        }
                        let norm = parse_u16(obj, &mut i);
                        while obj[i] != b' ' {
                            i += 1;
                        }
                        i += 1;
                        let b = parse_u16(obj, &mut i);
                        while obj[i] != b' ' {
                            i += 1;
                        }
                        i += 1;
                        let c = parse_u16(obj, &mut i);
                        while obj[i] != b' ' {
                            i += 1;
                        }
                        i += 1;
                        let d = parse_u16(obj, &mut i);
                        while i < obj.len() && obj[i] != b'\n' {
                            i += 1;
                        }
                        quads[n] = [a - 1, b - 1, d - 1, c - 1];
                        quad_norms[n] = norm;
                        n += 1;
                    } else if count_u16(obj, i) == 3 {
                        i += 2;
                        let a = parse_u16(obj, &mut i);
                        while obj[i] != b'/' {
                            i += 1;
                        }
                        while obj[i] != b'/' {
                            i += 1;
                        }
                        let norm = parse_u16(obj, &mut i);
                        while obj[i] != b' ' {
                            i += 1;
                        }
                        i += 1;
                        let b = parse_u16(obj, &mut i);
                        while obj[i] != b' ' {
                            i += 1;
                        }
                        i += 1;
                        let c = parse_u16(obj, &mut i);
                        while i < obj.len() && obj[i] != b'\n' {
                            i += 1;
                        }
                        tris[m] = [a - 1, b - 1, c - 1];
                        tri_norms[m] = norm;
                        m += 1;
                    }
                }
                i += 1;
            }
            Faces {
                quads,
                tris,
                quad_norms,
                tri_norms,
            }
        };
        Obj::<NUM_VERTICES, NUM_NORMALS, NUM_QUADS, NUM_TRIS, NUM_FACES> {
            vertices: unsafe { &mut VERTICES },
            normals: unsafe { &mut NORMALS },
            tris: unsafe { &mut FACES.tris },
            quads: unsafe { &mut FACES.quads },
            tri_norms: unsafe { &mut FACES.tri_norms },
            quad_norms: unsafe { &mut FACES.quad_norms },
        }
    }};
}
