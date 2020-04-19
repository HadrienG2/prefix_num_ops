use prefix_num_ops::bounded;

#[test]
fn min_value() {
    assert_eq!(bounded::min_value::<usize>(), usize::min_value());
}

#[test]
fn max_value() {
    assert_eq!(bounded::max_value::<usize>(), usize::max_value());
}
