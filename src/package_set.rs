use crate::{package::Package, version_set::VersionSet};

pub trait PackageSet {
    type P: Package;
    type VS: VersionSet;

    fn package(&self) -> Self::P;
    fn version_set(&self) -> Self::VS;
    fn new(package: Self::P, version_set: Self::VS) -> Self
    where
        Self: Sized;
}

pub struct DefaultPackageSet<P, VS> {
    package: P,
    version_set: VS,
}

impl<P: Package, VS: VersionSet> PackageSet for DefaultPackageSet<P, VS> {
    type P = P;
    type VS = VS;

    fn package(&self) -> Self::P {
        return self.package;
    }
    fn version_set(&self) -> Self::VS {
        return self.package;
    }
    fn new(package: Self::P, version_set: Self::VS) -> Self {
        Self {
            package,
            version_set,
        }
    }
}
