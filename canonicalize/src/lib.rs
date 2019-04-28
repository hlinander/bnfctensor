use std::collections::HashMap;
use std::collections::HashSet;
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

        let foobar = &Perm::from(vec![1, 2, 3, 4, 5, 6, 11, 8, 9, 12, 13, 0, 7, 10]);
        println!(
            "pi {:?}, pi^2 {:?}, pi^4 {:?}",
            foobar,
            &(foobar * foobar),
            &(&(foobar * foobar) * foobar) * foobar
        );
        println!("sgs: {:?}", sgs);
    }

    #[test]
    fn knuth_dense_pg() {
        let n = 10;
        let mut gs: Vec<Perm> = Vec::new();
        for i in 0..(n - 1) as u64 {
            let perm = Perm::from(Cycle::from((&vec![i, i + 1], n as u64)));
            gs.push(perm);
        }
        println!("{:?}", gs);
        let mut pg = PermGroup::init(n);
        for g in gs.clone() {
            pg.extend(n - 1, g);
        }
        assert_eq!(&pg.ts[n - 1], &gs);
        for k in 0..n {
            for j in 0..k {
                assert!(pg.gs.contains_key(&(k, j)));
            }
        }
    }

    #[test]
    fn test_stabilizer() {
        let n = 100;
        let cycles = vec![vec![0, 1], vec![1, 2], vec![2, 3], vec![3, 4]];

        let base = Vec::from_iter((0..n).rev());

        let perms: Vec<Perm> = cycles
            .into_iter()
            .map(move |x| Cycle::from((&x, n)))
            .map(Perm::from)
            .collect();

        let group = complete(&perms);

        let stab = |p| {
            let mut retn: Vec<Perm> = Vec::new();
            for g in &group {
                if g.image(p) == p {
                    retn.push(g.clone());
                }
            }
            retn
        };

        let sgs = create_sgs(GeneratingSet { g: perms });
        let complete_sgs = complete(&sgs.g);
        let hash_sgs: HashSet<Perm> = HashSet::from_iter(complete_sgs.iter().cloned());

        for i in 1..n as usize {
            let bs = base.iter().take(i);
            let mut stabs = Vec::new();
            for b in bs {
                stabs.push(HashSet::from_iter(stab(*b).into_iter()));
            }
            let s_b: HashSet<Perm> = stabs.iter().fold(stabs[0].clone(), |acc, x| {
                acc.intersection(&x).cloned().collect()
            });
            let sgs_intersect_stab: HashSet<&Perm> = hash_sgs.intersection(&s_b).collect();
            assert!(sgs_intersect_stab.len() >= s_b.len());
        }
    }

    #[test]
    fn perm_orbit() {
        let perm = Perm::from(vec![1, 2, 3, 0]);
        let origin = 0;
        let expected = vec![0, 1, 2, 3];
        let (orbit, _) = perm.orbit(origin);
        assert_eq!(orbit, expected);
    }

    #[test]
    fn test_point_outside_orbit_schreier_trace() {
        let perm = Perm::from(vec![1, 2, 3, 0, 5, 4]);
        let origin = 0;
        let gamma = 4;
        let (orbit, schreier) = perm.orbit(origin);
        let gen = schreier_trace(perm.v.len(), gamma, orbit.clone(), schreier);
        assert_eq!(gen, None);
    }

    #[test]
    fn test_schreier_trace() {
        let perm = Perm::from(vec![1, 2, 3, 0, 5, 4]);
        let origin = 4;
        let gamma = 5;
        let (orbit, schreier) = perm.orbit(origin);
        let omega = schreier_trace(perm.v.len(), gamma, orbit.clone(), schreier).unwrap();
        assert_eq!(omega.image(orbit[0]), gamma);
    }

    #[test]
    fn perm_schreier_orbit() {
        let perm = Perm::from(vec![1, 2, 3, 0, 4]);
        let origin = 0;
        let (_, schreier) = perm.orbit(origin);
        assert_eq!(schreier[0], Some(&perm));
        assert_eq!(schreier[1], Some(&perm));
        assert_eq!(schreier[2], Some(&perm));
        assert_eq!(schreier[3], Some(&perm));
        assert_eq!(schreier[4], None);
    }

    #[test]
    fn generator_set_orbit() {
        let gs = GeneratingSet {
            g: vec![
                Perm::from(vec![1, 2, 3, 0, 4, 5, 6, 7]),
                Perm::from(vec![0, 1, 2, 3, 5, 6, 7, 4]),
            ],
        };
        let origin1 = 0;
        let origin2 = 4;
        let expected1 = vec![0, 1, 2, 3];
        let expected2 = vec![4, 5, 6, 7];
        let (orbit1, _) = gs.orbit(origin1);
        let (orbit2, _) = gs.orbit(origin2);
        assert_eq!(orbit1, expected1);
        assert_eq!(orbit2, expected2);
    }

    #[test]
    fn test_canonicalize_free_butler() {
        let g = vec![Perm::from(vec![1, 0, 2]), Perm::from(vec![0, 2, 1])];
        let vinst1 = canonicalize_free_butler(&g, Perm::from(vec![2, 1, 0]));
        let vinst2 = canonicalize_free_butler(&g, Perm::from(vec![1, 0, 2]));
        let vinst3 = canonicalize_free_butler(&g, Perm::from(vec![1, 2, 0]));
        assert_eq!(vinst1, Perm::from(vec!(0, 1, 2)));
        assert_eq!(vinst2, Perm::from(vec!(0, 1, 2)));
        assert_eq!(vinst3, Perm::from(vec!(0, 1, 2)));
    }

    // #[test]
    fn test_canonicalize_free_butler_example7() {
        let g = vec![Perm::from(vec![0, 1, 3, 2]), Perm::from(vec![2, 3, 0, 1])];
        // let sgs = create_sgs(GeneratingSet{ g: gs.clone() });
        let vinst = canonicalize_free_butler(&g, Perm::from(vec![1, 2, 0, 3]));
        assert_eq!(vinst, Perm::from(vec!(0, 3, 2, 1)));
    }
}

#[no_mangle]
pub extern "C" fn canonicalize(perm: *const perm_t, out: *mut u8) -> () {
    unsafe {
        println!("HELLO RUST");
        // *out = 0x10;
    }
}

enum PermPointer<'a> {
    VecRef(*const Vec<Perm>, usize),
    Direct(&'a Perm),
}

impl<'a> PermPointer<'a> {
    fn get_ref<'b>(&'b self) -> &'b Perm {
        match self {
            PermPointer::VecRef(group, i) =>{
                unsafe {
                    (**group).get_unchecked(*i)
                }
            },
            PermPointer::Direct(perm) =>{
                perm
            }
        }
    }
}

use std::hash::{Hash, Hasher};
impl<'a> Hash for PermPointer<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.get_ref().hash(state);
    }
}
impl<'a> PartialEq for PermPointer<'a> {
    fn eq(&self, other: &PermPointer) -> bool {
        self.get_ref() == other.get_ref()
    }
}
impl<'a> Eq for PermPointer<'a> {}

fn complete(perms: &Vec<Perm>) -> Vec<Perm> {
    let mut group = perms.clone();
    let n = perms[0].v.len();
    let group_ptr = &group as *const _;
    let mut members: HashSet<PermPointer> = HashSet::from_iter(group.iter().enumerate().map(|(i, _)| PermPointer::VecRef(group_ptr, i)));
    let mut i = 0;
    while i < group.len() {
        for j in 0..group.len() {
            let next = &group[i] * &group[j];
            if !members.contains(&PermPointer::Direct(&next)) && next != Perm::identity(n as usize) {
                group.push(next);
                members.insert(PermPointer::VecRef(group_ptr, group.len() - 1));
            }
        }
        i += 1;
    }
    group
}

struct Cycle {
    n: u64,
    c: Vec<u64>,
}

struct PermGroup {
    b: Vec<Point>,
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

fn canonicalize_free_butler(gs: &Vec<Perm>, pi: Perm) -> Perm {
    let m = pi.v.len();
    let sgs = create_sgs(GeneratingSet { g: gs.clone() });
    let mut lambda = pi;
    let mut local_sgs = sgs.clone();
    let base = (0..m as Point).rev();
    for i in base {
        let (delta, schreier) = local_sgs.orbit(i);
        let (k, _): (usize, u64) = delta
            .iter()
            .map(|p| lambda.image(*p))
            .enumerate()
            .max_by(|(_, v1), (_, v2)| v1.cmp(v2))
            .unwrap();
        let p: u64 = delta[k];
        let omega = schreier_trace(m, p, delta, schreier).unwrap();
        lambda = omega * lambda;
        local_sgs.g = local_sgs
            .g
            .into_iter()
            .filter(|perm| perm.v[i as usize] == i)
            .collect();
        if local_sgs.g.len() == 0 {
            return lambda;
        }
    }
    lambda
}

fn create_sgs(gs: GeneratingSet) -> GeneratingSet {
    let n = gs.g[0].v.len();
    let mut pg = PermGroup::init(n);
    for g in gs.g {
        pg.extend(n - 1, g);
    }

    let mut retn = Vec::new();
    for t in pg.ts {
        for p in t {
            if !retn.contains(&p) {
                retn.push(p);
            }
        }
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
        let b = Vec::from_iter((0..n as Point).rev());

        PermGroup { b, gs, js, ts, cs }
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
        if self.is_member(&perm, k - 1) {
            return;
        }
        self.extend(k - 1, perm);
    }

    fn is_member(&self, perm: &Perm, k: usize) -> bool {
        let j = perm.v[k] as usize;
        if !self.gs.contains_key(&(k, j)) {
            return false;
        }
        if k == 0 {
            return true;
        }
        let sigma = &self.gs[&(k, j)];
        self.is_member(&(perm * &sigma.inverse()), k - 1)
    }
}

#[derive(Eq, PartialEq, Clone, Debug, Hash)]
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
            // 0 1 2 3 4 5 6 7 8 9
            // 4 2 3 1 0 5 8 6 7 9
            // retn[i] = v[self.v[i] as usize];
            //  ^--Image index 0 is index i1
            retn[self.v[i] as usize] = v[i];
        }
        retn
    }

    fn image(&self, p: u64) -> u64 {
        self.v[p as usize]
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

type Point = u64;

fn schreier_trace(
    n: usize,
    p: Point,
    orbit: Vec<Point>,
    schreier: Vec<Option<&Perm>>,
) -> Option<Perm> {
    if p == orbit[0] {
        return Some(Perm::identity(n));
    } else {
        let gen = schreier[p as usize]?;
        let tail = schreier_trace(n, gen.inverse().image(p), orbit, schreier)?;
        Some(&tail * gen)
    }
}

trait Generator {
    fn orbit(&self, point: Point) -> (Vec<Point>, Vec<Option<&Perm>>);
}

impl Generator for Perm {
    fn orbit(&self, origin: Point) -> (Vec<Point>, Vec<Option<&Perm>>) {
        let mut orbit = vec![origin];
        let mut schreier = vec![None; self.v.len()];
        let mut point = self.image(origin);
        schreier[origin as usize] = Some(self);
        while point != origin {
            orbit.push(point);
            schreier[point as usize] = Some(self);
            point = self.image(point);
        }
        (orbit, schreier)
    }
}

impl Generator for GeneratingSet {
    fn orbit(&self, origin: Point) -> (Vec<Point>, Vec<Option<&Perm>>) {
        let mut orbit = vec![origin];
        let n = self.g[0].v.len();
        let mut i = 0;
        let mut schreier = vec![None; n];
        // for perm in &self.g {
        //     let (o, _) = perm.orbit(origin);
        //     for p in o {
        //         if !orbit.contains(&p) {
        //             orbit.push(p);
        //             schreier[p as usize] = Some(perm);
        //         }
        //     }
        // }
        while i < orbit.len() {
            let p = orbit[i];
            for g in &self.g {
                let image = g.image(p);
                if !orbit.contains(&image) {
                    orbit.push(image);
                    schreier[image as usize] = Some(g);
                }
            }
            i += 1;
        }
        (orbit, schreier)
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
    // (p1 * p2) @ x = p2 @ (p1 @ x)
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
    // (p1 * p2) @ x = p2 @ (p1 @ x)
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

// impl ops::Mul for Option<Perm> {
//     type Output = Option<Perm>;
//     // p_i_j * p_j_k => p_i_k
//     // (p1 * p2) @ x = p1 @ (p2 @ x)
//     // [i1 i2 i3 ... ] [j1 j2 j3 ... ] {1 2 3 4 5}
//     //  ^- Image of 1 is i1
//     fn mul(self, rh: Option<Perm>) -> Option<Perm> {
//         let mut retn = Perm::identity(self.v.len());
//         for i in 0..self.v.len() {
//             retn.v[i] = rh.v[self.v[i] as usize];
//         }
//         retn
//     }
// }

#[derive(PartialEq, Debug, Clone)]
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
