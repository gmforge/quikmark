extern crate qwikmark;
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use qwikmark::document;

pub fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("document definition list", |b| {
        b.iter(|| document(black_box(": ab\n alpha\n: 12\n  digit\n  : ivn    roman")))
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
