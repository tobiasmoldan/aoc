use std::collections::{BTreeMap, BTreeSet};

use eyre::Result;

struct AntinodeIter {
    antennas: Vec<(usize, usize)>,
    i: usize,
    j: usize,
    second_pos: Option<(i32, i32)>,
}

impl AntinodeIter {
    pub fn new(antennas: Vec<(usize, usize)>) -> Self {
        Self {
            antennas,
            i: 0,
            j: 1,
            second_pos: None,
        }
    }
}

impl Iterator for AntinodeIter {
    type Item = (i32, i32);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(pos) = std::mem::take(&mut self.second_pos) {
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

        self.second_pos = Some((x2 + dx, y2 + dy));

        self.j += 1;
        if self.j == self.antennas.len() {
            self.i += 1;
            self.j = self.i + 1;
        }

        Some((x1 - dx, y1 - dy))
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
        .flat_map(AntinodeIter::new)
        .map(|(x, y)| {
            let Ok(x) = usize::try_from(x) else {
                return None;
            };
            let Ok(y) = usize::try_from(y) else {
                return None;
            };
            if x >= width {
                return None;
            }
            if y >= height {
                return None;
            }
            Some((x, y))
        })
        .collect::<BTreeSet<_>>()
        .len())
}
