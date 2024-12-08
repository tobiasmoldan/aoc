use std::collections::{BTreeMap, BTreeSet};

use eyre::Result;
struct AntinodeIter {
    antennas: Vec<(usize, usize)>,
    i: usize,
    j: usize,
    next: Vec<(usize, usize)>,
    height: usize,
    width: usize,
}

impl AntinodeIter {
    pub fn new(height: usize, width: usize, antennas: Vec<(usize, usize)>) -> Self {
        Self {
            antennas,
            i: 0,
            j: 1,
            next: Default::default(),
            height,
            width,
        }
    }
}

impl Iterator for AntinodeIter {
    type Item = (usize, usize);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(pos) = self.next.pop() {
            return Some(pos);
        }

        if self.i >= self.antennas.len() - 1 {
            return None;
        }

        let (x1, y1) = self.antennas[self.i];
        let (x2, y2) = self.antennas[self.j];

        let x1 = x1 as i32;
        let y1 = y1 as i32;
        let x2 = x2 as i32;
        let y2 = y2 as i32;
        let dx = x2 - x1;
        let dy = y2 - y1;

        let mut c = 1;
        loop {
            let x = x1 - (dx * c);
            let y = y1 - (dy * c);
            if x < 0 || y < 0 || x >= self.width as i32 || y >= self.height as i32 {
                break;
            }
            self.next.push((x as usize, y as usize));
            c += 1;
        }

        let mut c = 1;
        loop {
            let x = x2 + (dx * c);
            let y = y2 + (dy * c);
            if x >= self.width as i32 || y >= self.height as i32 || x < 0 || y < 0 {
                break;
            }
            self.next.push((x as usize, y as usize));
            c += 1;
        }

        self.next.push((x2 as usize, y2 as usize));

        self.j += 1;
        if self.j == self.antennas.len() {
            self.i += 1;
            self.j = self.i + 1;
        }

        Some((x1 as usize, y1 as usize))
    }
}

pub fn run(
    width: usize,
    height: usize,
    antennas: impl Iterator<Item = (char, usize, usize)>,
) -> Result<usize> {
    let antennas: BTreeMap<char, Vec<(usize, usize)>> =
        antennas.fold(BTreeMap::new(), |mut map, (id, x, y)| {
            map.entry(id).or_default().push((x, y));
            map
        });

    Ok(antennas
        .into_values()
        .flat_map(|v| AntinodeIter::new(height, width, v))
        .collect::<BTreeSet<_>>()
        .len())
}
