{-# LANGUAGE FlexibleInstances #-}
 -- GHC_STATIC_OPTION_i=../src:../testsuite


module Main where

import DescriptionParser
import Test.HUnit
import System.Exit (exitFailure)

-- text = "Sources: LYNX-46, LYNX-133\r\n\r\nThe visual renderings of instruments should be representative of the time of day being simulated (Brighter during day darker during night).\r\n\r\nThese instrument systems will be modelled:\r\n||ABBR.||DESCRIPTION||LOCATION||REQUIREMENT||PEER REVIEWS COMPLETED?||\r\n|CDNU1|TACCO's Control, Display and Navigation Unit|Interseat Console|LYNX-80|NO|\r\n|CDNU2|Pilot's Control, Display and Navigation Unit|Interseat Console|LYNX-80|NO|\r\n|EDU1|Electronic Support Measures Display Unit|Center Panel|LYNX-221|NO|\r\n|EDU2|Electronic Support Measures Display Unit|Center Panel|LYNX-221|NO|\r\n|ECU|Electronic Support Measures Control Unit|Center Panel|LYNX-222|NO|\r\n|CTB1|Central Warning System Test Switch/Button|TACCO Panel|LYNX-82|NO|\r\n|CTB2|Central Warning System Test Switch/Button|Pilot Panel|LYNX-82|NO|\r\n|SLDU1|Standby Flight Instrument Lighting Dimmer Unit|TACCO Panel|LYNX-84|NO|\r\n|SLDU2|Standby Flight Instrument Lighting Dimmer Unit|Pilot Panel|LYNX-84|NO|\r\n|SFI1|Standby Flight Instrument|TACCO Panel|LYNX-52|NO|\r\n|SFI2|Standby Flight Instrument|Pilot Panel|LYNX-52|NO|\r\n|LH1|'LOW HGT' Repeater Light|TACCO Panel|LYNX-86|NO|\r\n|LH2|'LOW HGT' Repeater Light|Pilot Panel|LYNX-86|NO|\r\n|CC1|'CYC/COLL' Repeater Light|TACCO Panel|LYNX-88|NO|\r\n|CC2|'CYC/COLL' Repeater Light|Pilot Panel|LYNX-88|NO|\r\n|BR1|'BAR/RAD' Repeater Light|TACCO Panel|LYNX-90|NO|\r\n|BR2|'BAR/RAD' Repeater Light|Pilot Panel|LYNX-90|NO|\r\n|MWL1|Master Warning Light|TACCO Panel|LYNX-54|NO|\r\n|MWL2|Master Warning Light|Pilot Panel|LYNX-54|NO|\r\n|DLEI1|Deck Lock Engaged Indicator|TACCO Panel|LYNX-92|NO|\r\n|DLEI2|Deck Lock Engaged Indicator|Pilot Panel|LYNX-92|NO|\r\n|MCL1|Master Caution Light|TACCO Panel|LYNX-49|NO|\r\n|MCL2|Master Caution Light|Pilot Panel|LYNX-49|NO|\r\n|IDU1|Integrated Display Unit|TACCO Panel|LYNX-1|NO|\r\n|IDU2|Integrated Display Unit|Center Panel|LYNX-1|NO|\r\n|IDU3|Integrated Display Unit|Center Panel|LYNX-1|NO|\r\n|IDU4|Integrated Display Unit|Pilot Panel|LYNX-1|NO|\r\n|CLK|Clock|Center Panel|LYNX-96|NO|\r\n|FCP|Flotation Control Panel|Center Panel|LYNX-94|NO|\r\n|EPSI1|Electronic Power Systems Instrument|Center Panel|LYNX-50|NO|\r\n|EPSI2|Electronic Power Systems Instrument|Center Panel|LYNX-50|NO|\r\n|ETB|EPSI Transfer Button|Center Panel|LYNX-183|NO|\r\n|CWP|Central Warning Panel|Center Panel|LYNX-48|NO|\r\n|AFCSC|AFCS Controller|Interseat Console|LYNX-99|NO|\r\n|ASWC|ASW Controller|Interseat Console|LYNX-98|NO|\r\n|VSCU|V/UHF1 Standby Control Unit|Interseat Console|LYNX-45|NO|\r\n|ECPL|Engine Control Panel|Overhead Console|LYNX-101|NO|\r\n|ECP|Emergency Control Panel|Interseat Console|LYNX-51|NO|\r\n|ASP/ASW?|Ancillary Switch Panel|Interseat Console|LYNX-185|NO|\r\n|MSP|Miscellaneous Switch Panel|Overhead Console|LYNX-231|NO|\r\n|ASB|Avionics Switch Box|Overhead Console|LYNX-233|NO|\r\n|SC|Standby Compass|Centre Windscreen Pillar|LYNX-186|NO|\r\n|ITB1|TACCO's IDU Transfer Button|Center Panel|LYNX-286|NO|\r\n|ITB2|Pilot's IDU Transfer Button|Center Panel|LYNX-286|NO|\r\n|ACU1|TACCO's Audio Control Unit|Interseat Console|LYNX-289|NO|\r\n|ACU2|Pilot's Audio Control Unit|Interseat Console|LYNX-289|NO|\r\n|LDP1|Lighting Dimmer Panel|Overhead Console|LYNX-301|NO|\r\n|LDP2|Lighting Dimmer Panel|Overhead Console|LYNX-301|NO|\r\n|JMSS|Jettison Mode Selector Switch|Center Panel|LYNX-315|NO|\r\n|MCP|Mission Control Panel|Interseat Console|LYNX-318|NO|\r\n|ESP|Electrical Systems Panel|Overhead Console|LYNX-329|NO|\r\n|FSP|Fuel Systems Panel|Overhead Console|LYNX-348|NO|\r\n|EFP|Engine Fire Panel|Overhead Console|LYNX-350|NO|\r\n|CDF|Chelton Direction Finder|Interseat Console|LYNX-356|NO|\r\n|CPICCU|Crash Position Indicator Cockpit Control Unit|Interseat Console|LYNX-373|NO|\r\n|PPT|Privacy Pushbutton and Telebrief Indicator|Interseat Console|LYNX-375|NO|\r\n|IAM|(-) -Ice Accretion Meter-|Starboard (Pilot) Side Panel|LYNX-508|NO|\r\n|SP|(-) -Switch Panel-|Starboard (Pilot) Side Panel|LYNX-352|NO|\r\n|OTMP|(-) -OEI Traning Mode Panel-|Port (TACCO) Side Panel|LYNX-354|NO|\r\n|DTDR|(-) -Data Transfer Device Receptacle-|Interseat Console|LYNX-388|NO|\r\n|NOSC|(-) -Nosewheel Oleo Strut Cover-|Interseat Console|LYNX-390|NO|\r\n|ROMI|(-) -Rotor Overspeed Magnetic Indicator (ROMI)-|Overhead Console|LYNX-391|NO|\r\n|BTS|(-) -Blade tracking and general purpose AC/DC supply sockets.-|Port (TACCO) Side Door Panel|LYNX-394|NO|\r\n|ESSB|(-) -Emergency Safety Services Break-|Starboard (Pilot) Side Panel|LYNX-401|NO|\r\n|RFOS|(-) -Rescue Hoist DC Gen Failure Override Switch-|Overhead Console (Pilot's Side)|LYNX-409|NO|\r\n|MASS|(-) -Master Armament Safety Switch-|Center Panel|LYNX-414|NO|\r\n|ABAI|(-) -Armament Bus A Light Emitting Diode Indicator-|Center Panel|LYNX-415|NO|\r\n|ABBI|(-) -Armament Bus B Light Emitting Diode Indicator-|Center Panel|LYNX-416|NO|\r\n\r\n(-) Capabilities marked as -deleted- are excluded and will not be emulated."

main :: IO Int
main = do
   -- putStrLn $ show $ testP prsDesc $ filter (\c -> c /= '\r') text
   _ <- runTestTT descriptionParserTests
   exitFailure

