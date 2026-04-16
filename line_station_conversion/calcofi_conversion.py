import numpy as np
import unittest


"""
Conversion routines for lat/lon to CalCOFI line/station and
the inverse transform using the algorithms of
Eber, L.E., and  R.P. Hewitt. 1979. Conversion algorithms for the CALCOFI
    station grid. California Cooperative Oceanic Fisheries Investigations
    Reports 20:135-137
with corrections reported in
Weber, E.D. and T.J. Moore 2013. Corrected Conversion Algorithms for the
    CalCOFI Station Grid and Their Implementation in Several Computer
    Languages. California Cooperative Oceanic Fisheries Investigations
    Reports 54: 97-106.

ex.
latlon_to_station(-121.15, 34.15)
latlon_to_station([-121.15, -129.28], [34.15, 37.346])
station_to_latlon(80, 60)
station_to_latlon([50, 80], [120, 60])

Note that conversions can now be done with better speed and accuracy
by calling Proj from the pyproj module, e.g.:

import pyproj


cc = pyproj.Proj('+proj=calcofi +ellps=clrk66')
cc(-121.15, 34.15)
cc([-121.15, -129.28], [34.15, 37.346])

cc(80, 60, inverse=True)
cc([50, 80], [120, 60], inverse=True)
"""


def inverse_mercator(lat_mercator, iterations=3):
    lat_mercator = np.array(lat_mercator, dtype='float')
    lat_approximate = lat_mercator
    for _i in range(iterations):
        lat_approximate = 2 * (np.arctan(np.exp(np.deg2rad(lat_mercator) +
                               0.00676866 * np.sin(np.deg2rad(
                                   lat_approximate)))) * 180 / np.pi - 45)
    return(lat_approximate)


def to_mercator(latitude):
    latitude = np.array(latitude, dtype='float')
    y = np.rad2deg(np.log(np.tan(np.deg2rad(45 + latitude / 2))) -
                   0.00676866 * np.sin(np.deg2rad(latitude)))
    return(y)


def station_to_latlon(x, y=None):
    """
    x is line, y is station, or x is a matrix.
    x and y are numbers, lists, tuples, or numpy arrays
    """
    if y is None:
        line = x[:, 0]
        station = x[:, 1]
    else:
        line = x
        station = y

    line = np.array(line, dtype='float')
    station = np.array(station, dtype='float')

    # need reshape b/c single numbers could be wrapped in arrays
    if len(line.shape) == 0:
        line = line.reshape(1)

    if len(station.shape) == 0:
        station = station.reshape(1)

    reference_lat = 34.15 - 0.2 * (line - 80.0) * np.cos(np.deg2rad(30.0))
    latitude = (reference_lat - (station - 60.0) *
                np.sin(np.deg2rad(30.0)) / 15)
    L1 = ((to_mercator(latitude) - to_mercator(34.15)) *
          np.tan(np.deg2rad(30)))
    L2 = (((to_mercator(reference_lat) - to_mercator(latitude)) /
           (np.cos(np.deg2rad(30)) * np.sin(np.deg2rad(30)))))
    longitude = -1 * (L1 + L2 + 121.15)
    ans = np.vstack((longitude, latitude)).T
    if len(line) == 1:
        ans = ans[0]
    return(ans)


def latlon_to_station(x, y=None):
    """
    x and y are numbers, lists, tuples, or numpy arrays.
    x can be a matrix with y = None
    """
    if y is None:
        lon = x[:, 0]
        lat = x[:, 1]
    else:
        lon = x
        lat = y
    lon = np.array(lon, dtype='float')
    lat = np.array(lat, dtype='float')
    # need reshape b/c single numbers could be wrapped in arrays
    if len(lon.shape) == 0:
        lon = lon.reshape(1)
    if len(lat.shape) == 0:
        lat = lat.reshape(1)
    # assume we're in the western hemispere
    lon[lon > 180] = -1 * (lon[lon > 180] - 360)
    lon[lon < 0] = lon[lon < 0] * -1
    L1 = (to_mercator(lat) - to_mercator(34.15)) * np.tan(np.deg2rad(30))
    L2 = lon - L1 - 121.15
    merc_lat_reference = (L2 * np.cos(np.deg2rad(30)) *
                          np.sin(np.deg2rad(30)) + to_mercator(lat))
    reference_lat = inverse_mercator(merc_lat_reference)
    line = 80 - (reference_lat - 34.15) * 5 / np.cos(np.deg2rad(30))
    station = 60 + (reference_lat - lat) * 15 / np.sin(np.deg2rad(30))
    ans = np.vstack((line, station)).T
    if len(line) == 1:
        ans = ans[0]
    return(ans)


class CalcofiConversionTester(unittest.TestCase):
    def test_latlon_to_station(self):
        line, station = latlon_to_station(-121.15, 34.15)
        self.assertAlmostEqual(line, 80.0, delta=0.00001)
        self.assertAlmostEqual(station, 60.0, delta=0.00001)

        # test that they also work with arrays correctly
        ls = latlon_to_station([-121.15, -129.2795443], [34.15, 37.34615442])
        self.assertAlmostEqual(ls[1, 0], 50, delta=0.00001)
        self.assertAlmostEqual(ls[1, 1], 120, delta=0.00001)

        ls2 = latlon_to_station(
            np.array([-121.15, 34.15, -129.2795443, 37.34615442]).reshape(
                [2, 2]))
        np.testing.assert_array_equal(ls, ls2)

    def test_station_to_latlon(self):
        lon, lat = station_to_latlon(80, 60)
        self.assertAlmostEqual(lon, -121.15)
        self.assertAlmostEqual(lat, 34.15)

        ll = station_to_latlon([50, 80], [120, 60])
        self.assertAlmostEqual(ll[0, 0], -129.2795443)
        self.assertAlmostEqual(ll[0, 1], 37.34615242)

        ll2 = station_to_latlon(
            np.array([50, 120, 80, 60]).reshape(
                [2, 2]))
        np.testing.assert_array_equal(ll, ll2)


if __name__ == '__main__':
    unittest.main()
