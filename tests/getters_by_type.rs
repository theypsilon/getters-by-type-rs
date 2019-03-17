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
        assert_eq!(actual.get_mut_fields_incdec_bool__().len(), 1);
        assert_eq!(actual.get_mut_fields_pair_bool_bool__().len(), 1);
        assert_eq!(actual.get_fields_pair_incdec__bool__pair__i32_pair__i32_i32____().len(), 1);
    }

    #[test]
    fn test_getters_mut_by_type___on_nested_structs___have_correct_length() {
        #[derive(GettersMutByType, Default)]
        struct Test {
            boom: IncDec<IncDec<bool>>,
        }

        let actual = Test::default();
        assert_eq!(actual.get_fields_incdec_incdec__bool___()[0].get_fields_t().len(), 2);
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
}
