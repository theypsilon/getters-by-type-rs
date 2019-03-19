extern crate getters_by_type;

#[allow(non_snake_case)]
mod tests {
    use getters_by_type::{GettersByType, GettersMutByType};

    #[test]
    fn test_getters_by_type___always___have_correct_length() {
        #[derive(GettersByType)]
        struct Test<'a> {
            first: &'a bool,
            second: &'a mut bool,
            third: bool,
        }

        let foo = true;
        let mut bar = true;
        let actual = Test {
            first: &foo,
            second: &mut bar,
            third: true,
        };

        assert_eq!(actual.get_fields_bool().len(), 3);
    }

    #[test]
    fn test_getters_mut_by_type___with_ref_non_mut___have_correct_length() {
        #[derive(GettersMutByType)]
        struct Test<'a> {
            first: &'a bool,
            second: &'a mut bool,
            third: bool,
        }

        let foo = true;
        let mut bar = true;
        let mut actual = Test {
            first: &foo,
            second: &mut bar,
            third: true,
        };

        assert_eq!(actual.get_mut_fields_bool().len(), 2);
        assert_eq!(actual.get_fields_bool().len(), 3);
    }

    #[derive(GettersMutByType, Default, Debug, PartialEq)]
    struct IncDec<T> {
        inc: T,
        dec: T,
    }

    #[test]
    fn test_getters_mut_by_type___always___have_correct_length() {
        #[derive(GettersMutByType, Default)]
        struct Test {
            boom: IncDec<bool>,
            badaboom: Pair<IncDec<bool>, Pair<i32, Pair<i32, i32>>>,
            first: bool,
            second: bool,
            third: bool,
            foo: f32,
            bar: f32,
            length: i32,
            joder: i32,
            pair: Pair<bool, bool>,
        }

        #[derive(Default, Debug)]
        struct Pair<T, U> {
            first: T,
            second: U,
        }

        let mut actual = Test::default();
        assert_eq!(actual.get_mut_fields_bool().len(), 3);
        assert_eq!(actual.get_mut_fields_f32().len(), 2);
        assert_eq!(actual.get_mut_fields_i32().len(), 2);
        assert_eq!(actual.get_mut_fields_incdec_bool_().len(), 1);
        assert_eq!(actual.get_mut_fields_pair_bool_bool_().len(), 1);
        assert_eq!(actual.get_fields_pair_incdec_bool__pair_i32_pair_i32_i32___().len(), 1);
    }

    #[test]
    fn test_getters_mut_by_type___on_nested_structs___have_correct_length() {
        #[derive(GettersMutByType, Default)]
        struct Test {
            boom: IncDec<IncDec<bool>>,
        }

        let actual = Test::default();
        assert_eq!(actual.get_fields_incdec_incdec_bool__()[0].get_fields_t().len(), 2);
    }

    #[test]
    fn test_getters_by_type___when_called___contain_proper_value() {
        #[derive(GettersMutByType)]
        struct Test {
            first: i32,
            second: i32,
        }

        let actual = Test { first: 1, second: 10 };
        assert_eq!(*actual.get_fields_i32()[0], 1);
        assert_eq!(*actual.get_fields_i32()[1], 10);
    }

    #[test]
    fn test_getters_mut_by_type___when_changed___update_fields() {
        #[derive(GettersMutByType)]
        struct Test {
            value: i32,
        }

        let mut actual = Test { value: 1 };
        *actual.get_mut_fields_i32()[0] = 20;
        assert_eq!(actual.value, 20);
    }

    #[test]
    fn test_getters_by_type___with_box_fn_types___compiles_fine() {
        #[derive(GettersByType)]
        struct Test {
            action: Box<Fn(i32) -> f32>,
        }

        let actual = Test { action: Box::new(|_| 0.0) };
        assert_eq!(actual.get_fields_box_fn_i32_f32_().len(), 1);
    }

    #[test]
    fn test_getters_by_type___with_fn_types___compiles_fine() {
        #[derive(GettersByType)]
        struct Test {
            op: fn(i32) -> f32,
        }
        fn op(_: i32) -> f32 {
            0.0
        }
        let actual = Test { op };
        assert_eq!(actual.get_fields_fn_i32_f32().len(), 1);
    }
}
