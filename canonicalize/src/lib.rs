use std::collections::HashMap;
use std::iter::FromIterator;
use std::ops;

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }

    #[test]
    fn multiply_two_identies() {
        let lh = Perm::identity(10);
        let rh = Perm::identity(10);
        assert_eq!(&lh * &rh, Perm::identity(10));
        assert!(&rh * &lh != Perm::identity(11));
    }

    #[test]
    fn multiply_one_identity() {
        let lh = Perm::from(vec![0, 2, 4, 1, 3]);
        let rh = Perm::identity(5);
        assert_eq!(&lh * &rh, lh);
    }

    #[test]
    fn multiply_permutation() {
        let set = vec![199, 22, 310, 44, 516];
        let p1 = Perm::from(vec![3, 4, 2, 0, 1]);
        let p2 = Perm::from(vec![0, 3, 1, 4, 2]);
        let mul = &p1 * &p2;

        assert_eq!(mul.permute(&set), p2.permute(&p1.permute(&set)));
    }
    #[test]
    fn permute() {
        let expected = vec![516, 44, 310, 22, 199];
        let input = vec![199, 22, 310, 44, 516];
        let p = Perm::from(vec![4, 3, 2, 1, 0]);
        assert_eq!(p.permute(&input), expected);
    }

    #[test]
    fn invert_permute() {
        let expected = vec![199, 22, 310, 44, 516];
        let p = Perm::from(vec![4, 3, 2, 1, 0]);
        assert_eq!(p.inverse().permute(&p.permute(&expected)), expected);
    }

    #[test]
    fn invert_multiply_permute() {
        let expected = vec![199, 22, 310, 44, 516];
        let p = Perm::from(vec![4, 3, 2, 1, 0]);
        assert_eq!((&p.inverse() * &p).permute(&expected), expected);
    }

    #[test]
    fn knuth_sparse_sgs() {
        // π = [1,2,3,4,5,6,7,14] [8,9,10,13] [11,12]
        let n = 14;
        let cycles = [
            vec![0, 1, 2, 3, 4, 5, 6, 11],
            vec![7, 8, 9, 12],
            vec![10, 13],
        ];
        let foo: Perm = cycles
            .into_iter()
            .map(move |x| Cycle::from((x, n)))
            .map(Perm::from)
            .fold(Perm::identity(n as usize), |a, b| &a * &b);

        println!("gs: {:?}", foo);

        let sgs = create_sgs(GeneratingSet { g: vec![foo] });

        let foobar = &Perm::from(vec!(1, 2, 3, 4, 5, 6, 11, 8, 9, 12, 13, 0, 7, 10));
        println!("pi {:?}, pi^2 {:?}, pi^4 {:?}", foobar, &(foobar*foobar), &(&(foobar * foobar) * foobar) * foobar);
        println!("sgs: {:?}", sgs);
    }

    #[test]
    fn knuth_dense_pg() {
        let n = 10;
        let mut gs: Vec<Perm> = Vec::new();
        for i in 0..(n - 1) as u64 {
            let perm = Perm::from(Cycle::from((&vec!(i, i + 1), n as u64)));
            gs.push(perm);
        }
        println!("{:?}", gs);
        let mut pg = PermGroup::init(n);
        for g in gs.clone() {
            pg.extend(n - 1, g);
        }
        assert_eq!(&pg.ts[n-1], &gs);
        for k in 0..n {
            for j in 0..k {
                assert!(pg.gs.contains_key(&(k, j)));
            }
        }

    }
}

#[no_mangle]
pub extern "C" fn canonicalize(perm: *const perm_t, out: *mut u8) -> () {
    unsafe {
        println!("HELLO RUST");
        // *out = 0x10;
    }
}

struct Cycle {
    n: u64,
    c: Vec<u64>,
}

struct PermGroup {
    gs: HashMap<(usize, usize), Perm>,
    js: Vec<Vec<usize>>,
    ts: Vec<Vec<Perm>>,
    cs: Vec<Vec<u64>>,
}

// Algorithm Ak(π).
// Assuming that the data structure is up-to-date of order k, and that π ∈ Π(k)
// but π !∈ Γ(k), this procedure appends π to T(k) and brings the data structure
// back up-to-date so that Γ(k) will equal the new〈T(k)〉.
// Step A1. Insert π into the set T(k).
// Step A2. Perform algorithm Bk(στ) for all σ ∈ Σ(k) and τ ∈ T(k) such that στ
// is not already known to be a member of Γ(k).
//
// (Algorithm Bk may increase the size of Σ(k); any new perms σ that are added to Σ(k)
// must also be included in this step. Implementation details are discussed in
// Section 3 below.)
//
// Algorithm Bk(π). Assuming that the data structure is
// up-to-date of order k−1, and that π ∈〈T(k)〉, this procedure ensures that π is
// in Γ(k) and that the data structure remains up-to-date of order k−1. (The value
// of k will always be greater than 1.)
// Step B1. Let π take k|→j
// Step B2. If σkj = ∅, set σkj ← π and terminate the algorithm.
// Step B3. If πσ−kj ∈ Γ(k−1), terminate the algorithm. (This test for
// membership in Γ(k−1) has been described in Section 1 above.)
// Step B4. Perform algorithm Ak−1(πσ−kj).

// i←1;
// while i≤s(k):
//     while c(k, i)< t(k):
//         begin l←c(k, i) + 1;
//         Bk(σkj(k,i)τ(k, l));
//         c(k, i)←l;
//     i←i+ 1;

fn create_sgs(gs: GeneratingSet) -> GeneratingSet {
    let n = gs.g[0].v.len();
    let mut pg = PermGroup::init(n);
    for g in gs.g {
        pg.extend(n - 1, g);
    }

    let mut retn = Vec::new();
    for t in pg.ts {
        retn.extend(t);
    }
    GeneratingSet { g: retn }
}

impl PermGroup {
    fn init(n: usize) -> PermGroup {
        // let n = gs.g[0].v.len();
        let mut ts = Vec::new();
        for i in 0..n {
            ts.push(Vec::new());
        }
        let mut cs = Vec::new();
        for i in 0..n {
            let mut ccs = Vec::new();
            ccs.resize(n, 0);
            cs.push(ccs);
        }
        let mut js = Vec::new();
        for i in 0..n {
            js.push(Vec::new());
        }
        let mut gs = HashMap::new();
        for i in 0..n {
            gs.insert((i, i), Perm::identity(n));
            js[i].push(i);
        }

        PermGroup { gs, js, ts, cs }
    }
    fn extend(&mut self, k: usize, pi: Perm) {
        self.ts[k].push(pi);
        let mut i: usize = 0;
        while i < self.js[k].len() {
            while self.cs[k][i] < (self.ts[k].len() as u64) {
                let l = self.cs[k][i] + 1;
                let tau = &self.ts[k][(l - 1) as usize];
                let j = self.js[k][i];
                let sigma = &self.gs[&(k, j)];
                self.update(k, sigma * tau);
                self.cs[k][i] = l;
            }
            i += 1;
        }
    }

    fn update(&mut self, k: usize, pi: Perm) {
        let j = pi.v[k] as usize;
        if !self.gs.contains_key(&(k, j)) {
            self.gs.insert((k, j), pi);
            self.js[k].push(j);
            return;
        }
        let sigma = &self.gs[&(k, j)];
        let perm = &pi * &sigma.inverse();
        if self.is_member(&perm, k) {
            return;
        }
        self.extend(k - 1, perm);
    }

    fn is_member(&self, perm: &Perm, k: usize) -> bool {
        let j = perm.v[k] as usize;
        if !self.gs.contains_key(&(k, j)) {
            return false;
        }
        if k == 1 {
            return true;
        }
        let sigma = &self.gs[&(k, j)];
        self.is_member(&(perm * &sigma.inverse()), k - 1)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct Perm {
    v: Vec<u64>,
}

impl Perm {
    fn identity(len: usize) -> Perm {
        let r = 0..len as u64;
        Perm {
            v: Vec::from_iter(r),
        }
    }

    fn permute(&self, v: &Vec<u64>) -> Vec<u64> {
        let length = v.len();
        let mut retn = Vec::from_iter(0..length as u64);
        for i in 0..length {
            // retn[i] = v[self.v[i] as usize];
            // [i1 i2 ...] {0 1 2 3}
            //  ^--Image index 0 is index i1
            retn[self.v[i] as usize] = v[i];
        }
        retn
    }

    fn inverse(&self) -> Perm {
        let length = self.v.len();
        let mut retn = Vec::from_iter(0..length as u64);
        for i in 0..length {
            // [i1 i2 ...]^-
            //  ^--Image of index i1 is index 0
            retn[self.v[i] as usize] = i as u64;
        }
        Perm { v: retn }
    }
}

impl From<Vec<u64>> for Perm {
    fn from(v: Vec<u64>) -> Perm {
        Perm { v }
    }
}

impl ops::Mul for &Perm {
    type Output = Perm;
    // p_i_j * p_j_k => p_i_k
    // (p1 * p2) @ x = p1 @ (p2 @ x)
    // [i1 i2 i3 ... ] [j1 j2 j3 ... ] {1 2 3 4 5}
    //  ^- Image of 1 is i1
    fn mul(self, rh: &Perm) -> Perm {
        let mut retn = Perm::identity(self.v.len());
        for i in 0..self.v.len() {
            retn.v[i] = rh.v[self.v[i] as usize];
        }
        retn
    }
}

impl ops::Mul for Perm {
    type Output = Perm;
    // p_i_j * p_j_k => p_i_k
    // (p1 * p2) @ x = p1 @ (p2 @ x)
    // [i1 i2 i3 ... ] [j1 j2 j3 ... ] {1 2 3 4 5}
    //  ^- Image of 1 is i1
    fn mul(self, rh: Perm) -> Perm {
        let mut retn = Perm::identity(self.v.len());
        for i in 0..self.v.len() {
            retn.v[i] = rh.v[self.v[i] as usize];
        }
        retn
    }
}

#[derive(PartialEq, Debug)]
struct GeneratingSet {
    g: Vec<Perm>,
}

struct TraversalSet {
    t: Vec<Perm>,
}

impl From<Cycle> for Perm {
    fn from(cs: Cycle) -> Perm {
        let mut p = Perm::identity(cs.n as usize);
        if cs.c.len() < 2 {
            return p;
        }
        assert!(cs.c.len() >= 2);
        let mut loff = cs.c.clone();
        loff.push(loff[0]);
        // π = [1,2,3,4,5,6,7,14] [8,9,10,13] [11,12]
        for s in loff.as_slice().windows(2) {
            match s {
                &[i, j] => p.v[i as usize] = j,
                _ => unreachable!(),
            }
        }
        p
    }
}

impl From<(&Vec<u64>, u64)> for Cycle {
    fn from((c, n): (&Vec<u64>, u64)) -> Cycle {
        let roffe = c.clone();
        Cycle { c: roffe, n }
    }
}

#[repr(C)]
pub struct perm_t {
    lengthh: u64,
    dummies_length: u64,
    frees_length: u64,
    window_size: u64,
    perm: *const u64,
    generating_set: *const u64,
    frees: *const u64,
    dummies: *const u64,
}
