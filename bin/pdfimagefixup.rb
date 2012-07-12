#!/usr/bin/env ruby           
# encoding: utf-8
require 'mini_magick'
require 'origami'
include Origami

def fixup!(pdf)
  pdf.root_objects.each do |obj|
    next unless obj.is_a?(Graphics::ImageXObject)

    Dir.mktmpdir do |dir|
      ext, data = obj.to_image_file

      tf = "#{dir}/tmp.#{ext}"
      open(tf, 'w:binary') {|io| io.print data }

      img = MiniMagick::Image.open(tf)
      cs = img['colorspace']

      img.combine_options do |c|
        if cs == 'DirectClassRGB'
          c.auto_level
        else
          c.level '20%,80%'
          c.sharpen '5'
          c.colorspace 'gray'
        end
        c.resize 'x1400'
      end

      img.write(tf)
      open(tf, 'r:binary') do |io|
        obj.rawdata = io.read
      end

      obj.Width, obj.Height = img['dimensions']
    end
  end
end

ARGV.each do |fn|
  pdf = PDF.read(fn)
  fixup!(pdf)
  pdf.saveas("#{File.dirname(fn)}/t_#{File.basename(fn)}")
end
